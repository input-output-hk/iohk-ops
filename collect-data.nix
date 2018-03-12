{ 
  coreNodes          # the number of core nodes
, startWaitTime      # how many minutes to wait for the cluster to start
, time               # number of transactions that each thread should send
, conc               # number of each generator's threads
, delay              # number of milliseconds to wait between each send
, sendMode
, cooldown           # number of minutes to wait for cooldown
, addGenerators      # using more than one generator increases the load for stress-tests
, walletsDeployment  # edge nodes' deployment
}:

with import <nixpkgs> {};
writeScriptBin "collect-data.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=`grep name: config.yaml | awk '{print $2}'`

  export TZ=UTC

  CORENODES=${coreNodes} # the number of core nodes.
  # When changing the number of core nodes, you need to:
  # - adjust the topology file
  # - adjust lib/configuration.yaml in cardano-sl
  #   change richmen in the bench section
  #   commit the change, and push
  # - destroy the cluster and re-set it with the new commit
  START_WAIT_TIME=${startWaitTime} # how many minutes to wait for the cluster to start
                                   # before starting the transaction generator
  TIME=${time}                     # number of transactions that each thread should send
  CONC=${conc}                     # number of threads
  DELAY=${delay}                   # number of milliseconds to wait between each send
  SENDMODE=${sendMode}
  COOLDOWN=${cooldown}             # number of minutes to wait for cooldown
  ADDGENERATORS=${addGenerators}   # using more than one generator might help increase the
                                   # load for stress-tests
  WALLETS_DEPLOYMENT=${walletsDeployment} 
                                   # edge nodes can be added for further investigation

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  # build needed tools
  nix-build -A cardano-sl-tools -o post-mortem # for cardano-post-mortem

  TOPOLOGY=`grep topology: config.yaml | awk '{print $2}'`
  CONFIGURATIONYAML=`nix-instantiate -E '(import ./. {}).cardano-sl.src + "/configuration.yaml"' --eval`

  SYSTEMSTART=`grep -A 2 systemStart config.yaml | grep contents | awk '{print $2}'`

  $IO dumplogs nodes

  # timestamp identifying this run
  LAST=`ls experiments/ | grep 20 | sort | tail -1`
  LOGDIR="experiments/$LAST"
  # commit of cardano-sl
  COMMIT=`grep rev cardano-sl-src.json | awk '{print $2}' | cut -c 2- | cut -c -7`
  # cluster topology file
  TOPOLOGY=`grep topology: config.yaml | awk '{print $2}'`

  # archive settings and topology file
  echo "commit=$COMMIT, sendmode=$SENDMODE, time=$TIME, conc=$CONC, delay=$DELAY, generators=$((ADDGENERATORS + 1)), systemstart=$SYSTEMSTART" > $LOGDIR/bench-settings
  cp $TOPOLOGY $LOGDIR/

  # parse slot duration and start time from tx generator output
  SLOTDURATION=`grep -oP '(?<=slotDuration=)[0-9]+' tps-sent.csv`
  STARTTIME=`grep -oP '(?<=startTime=)[0-9]+' tps-sent.csv`

  # run post-mortem tool to parse json logs from nodes
  ./post-mortem/bin/cardano-post-mortem  overview 0.05 $LOGDIR

  # assemble csv file from tx generator and node logs
  TPSFILE="run-$LAST.csv"
  echo "time,txCount,txType,slotDuration,conc,sendMode,clustersize,startTime,commit,node,run,time,delay" >$TPSFILE
  # output from generators
  awk 'FNR>2{print $1,$2,$3,slotDuration,conc,sendMode,clustersize,startTime,commit,node,run,time,delay}' FS=, OFS=, \
      slotDuration=$SLOTDURATION \
      conc=$CONC \
      sendMode=$SENDMODE \
      clustersize=$CORENODES \
      startTime=$STARTTIME \
      commit=$COMMIT \
      node="generator" \
      run="$LAST" \
      time="$TIME" \
      delay="$DELAY" \
      tps-sent.csv >> $TPSFILE
  mv tps-sent.csv $LOGDIR
  for n in $(seq 1 $ADDGENERATORS); do
      awk 'FNR>2{print $1,$2,$3,slotDuration,conc,sendMode,clustersize,startTime,commit,node,run,time,delay}' FS=, OFS=, \
          slotDuration=$SLOTDURATION \
          conc=$CONC \
          sendMode=$SENDMODE \
          clustersize=$CORENODES \
          startTime=$STARTTIME \
          commit=$COMMIT \
          node="generator$n" \
          run="$LAST" \
          time="$TIME" \
          delay="$DELAY" \
          tps-sent-$n.csv >> $TPSFILE
      mv tps-sent-$n.csv $LOGDIR
  done
  # output from post-mortem analyser
  awk 'FNR>1{print $1,$2,$3,slotDuration,conc,sendMode,clustersize,startTime,commit,$4,run,time,delay}' FS=, OFS=, \
      slotDuration=$SLOTDURATION \
      conc=$CONC \
      sendMode=$SENDMODE \
      clustersize=$CORENODES \
      startTime=$STARTTIME \
      commit=$COMMIT \
      run="$LAST" \
      time="$TIME" \
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

  $(nix-build create-plots.nix --argstr last $LAST)/create-plots.sh

  tar cJf run-$LAST.tar.xz -C experiments/$LAST

  echo "--- Benchmarks finished. Find the results at"
  echo "    $PWD/run-$LAST.tar.xz"
  echo "    $PWD/$LOGDIR/plots/"
  echo "    $PWD/$LOGDIR/$TPSFILE"
  echo "    $PWD/$LOGDIR/report_$LAST.txt"
  echo "    $LOGDIR"
''
