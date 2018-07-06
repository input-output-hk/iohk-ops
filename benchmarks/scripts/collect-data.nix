{
  coreNodes          # the number of core nodes
, startWaitTime      # how many minutes to wait for the cluster to start
, txsPerThread       # number of transactions that each thread should send
, conc               # number of each generator's threads
, delay              # number of milliseconds to wait between each send
, cooldown           # number of minutes to wait for cooldown
, addGenerators      # using more than one generator increases the load for stress-tests
, edgeNodes          # the number of edge nodes (wallets)
                     # must be a multiple of 10
, corePolicy         # policy file for core nodes
, relayPolicy        # policy file for relay nodes
}@args:

with import <nixpkgs> {};
writeScriptBin "collect-data.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=`grep name: ../config.yaml | awk '{print $2}'`

  export TZ=UTC

  CORENODES=${coreNodes} # the number of core nodes
  # When changing the number of core nodes, you need to:
  # - adjust the topology file
  # - adjust lib/configuration.yaml in cardano-sl
  #   change richmen in the bench section
  #   commit the change, and push
  # - destroy the cluster and re-set it with the new commit
  START_WAIT_TIME=${startWaitTime} # how many minutes to wait for the cluster to start
                                   # before starting the transaction generator
  TXS_PER_THREAD=${txsPerThread}   # number of transactions that each thread should send
  CONC=${conc}                     # number of threads
  DELAY=${delay}                   # number of milliseconds to wait between each send
  COOLDOWN=${cooldown}             # number of minutes to wait for cooldown
  ADDGENERATORS=${addGenerators}   # using more than one generator might help increase the
                                   # load for stress-tests
                                   # edge nodes can be added for further investigation
  EDGENODES=${edgeNodes}           # the number of edge nodes

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  # build needed tools
  nix-build -A cardano-sl-tools -o post-mortem # for cardano-post-mortem

  TOPOLOGY=`grep topology: ../config.yaml | awk '{print $2}'`
  CONFIGURATIONYAML=`nix-instantiate -E '(import ./. {}).cardano-sl.src + "/configuration.yaml"' --eval`

  K=`awk -v p1=bench ' $0 ~ p1 {found = 1} found && /k: [0-9]+/ { print $0; exit }' $CONFIGURATIONYAML | awk '{print $2}'`
  SLOT_DURATION=`awk -v p1=bench ' $0 ~ p1 {found = 1} found && /slotDuration:[ ]+[0-9]+/ { print $0; exit }' $CONFIGURATIONYAML | awk '{print $2}'`

  RATELIMIT=`awk -v p1=dequeue ' $0 ~ p1 {found = 1} found && /rateLimit: [0-9]+/ { print $0; exit }' ${relayPolicy} | awk '{print $2}'`

  SYSTEMSTART=`grep -A 2 systemStart ../config.yaml | grep contents | awk '{print $2}'`

  $IO -C .. dumplogs nodes

  # timestamp identifying this run
  LAST=`ls ../experiments/ | grep 20 | sort | tail -1`
  LOGDIR="experiments/$LAST"

  mkdir -p ./$LOGDIR
  mv ../$LOGDIR/* ./$LOGDIR
  rm -r ../experiments

  # commit of cardano-sl
  COMMIT=`grep rev ../cardano-sl-src.json | awk '{print $2}' | cut -c 2- | cut -c -7`
  # cluster topology file
  TOPOLOGY=`grep topology: ../config.yaml | awk '{print $2}'`

  # policy files
  CORE_POLICY=${corePolicy}
  RELAY_POLICY=${relayPolicy}

  # archive settings and topology file
  echo "commit=$COMMIT, txsPerThread=$TXS_PER_THREAD, k=$K, \
  slotDuration=$SLOT_DURATION ms, conc=$CONC, delay=$DELAY ms, \
  relay to core rateLimit=$RATELIMIT, generators=$((ADDGENERATORS + 1)), \
  edgenodes=$EDGENODES, systemstart=$SYSTEMSTART" > $LOGDIR/bench-settings
  cp ../$TOPOLOGY $LOGDIR

  # archive policy files
  cp $CORE_POLICY $LOGDIR
  cp $RELAY_POLICY $LOGDIR

  # parse slot duration and start time from tx generator output
  SLOTDURATION=`grep -oP '(?<=slotDuration=)[0-9]+' tps-sent.csv`
  STARTTIME=`grep -oP '(?<=startTime=)[0-9]+' tps-sent.csv`

  # run post-mortem tool to parse json logs from nodes
  ./post-mortem/bin/cardano-post-mortem  overview 0.05 $LOGDIR

  # assemble csv file from tx generator and node logs
  TPSFILE="run-$LAST.csv"
  echo "time,txCount,txType,slotDuration,conc,clustersize,startTime,commit,node,run,txsPerThread,delay" >$TPSFILE
  # output from generators
  awk 'FNR>2{print $1,$2,$3,slotDuration,conc,clustersize,startTime,commit,node,run,txsPerThread,delay}' FS=, OFS=, \
      slotDuration=$SLOTDURATION \
      conc=$CONC \
      clustersize=$CORENODES \
      startTime=$STARTTIME \
      commit=$COMMIT \
      node="generator" \
      run="$LAST" \
      txsPerThread="$TXS_PER_THREAD" \
      delay="$DELAY" \
      tps-sent.csv >> $TPSFILE
  mv tps-sent.csv $LOGDIR
  for n in $(seq 1 $ADDGENERATORS); do
      awk 'FNR>2{print $1,$2,$3,slotDuration,conc,clustersize,startTime,commit,node,run,txsPerThread,delay}' FS=, OFS=, \
          slotDuration=$SLOTDURATION \
          conc=$CONC \
          clustersize=$CORENODES \
          startTime=$STARTTIME \
          commit=$COMMIT \
          node="generator$n" \
          run="$LAST" \
          txsPerThread="$TXS_PER_THREAD" \
          delay="$DELAY" \
          tps-sent-$n.csv >> $TPSFILE
      mv tps-sent-$n.csv $LOGDIR
  done
  # output from post-mortem analyser
  awk 'FNR>1{print $1,$2,$3,slotDuration,conc,clustersize,startTime,commit,$4,run,txsPerThread,delay}' FS=, OFS=, \
      slotDuration=$SLOTDURATION \
      conc=$CONC \
      clustersize=$CORENODES \
      startTime=$STARTTIME \
      commit=$COMMIT \
      run="$LAST" \
      txsPerThread="$TXS_PER_THREAD" \
      delay="$DELAY" \
      csv_$LAST.csv >> $TPSFILE
  mv csv_$LAST.csv $LOGDIR

  # move files
  mv $TPSFILE $LOGDIR
  mv experiments/txgen $LOGDIR
  mv report_$LAST.txt $LOGDIR
  mv times.csv $LOGDIR/times.csv
  mv times.svg $LOGDIR/times.svg
  mv auxx-*.log $LOGDIR

  $(nix-build ./scripts/create-plots.nix --argstr last $LAST)/bin/create-plots.sh

  # save arguments with which nix-script was called
  cat <<EOF >collect-data-args.txt
 ${toString (lib.mapAttrsToList (name: value: name + "=" + value + "\n") args)}
EOF

  mv *-args.txt $LOGDIR

  tar cJf run-$LAST.tar.xz -C experiments/ $LAST

  echo "--- Benchmarks finished. Find the results at"
  echo "    $PWD/run-$LAST.tar.xz"
  echo "    $PWD/$LOGDIR/plots/"
  echo "    $PWD/$LOGDIR/$TPSFILE"
  echo "    $PWD/$LOGDIR/report_$LAST.txt"
  echo "    $LOGDIR"
''
