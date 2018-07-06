{
  coreNodes     ? "7"     # the number of core nodes
, startWaitTime ? "10"    # how many minutes to wait for the cluster to start
, txsPerThread  ? "500"   # number of transactions that each thread should send
, conc          ? "1"     # number of each generator's threads
, delay         ? "250"   # number of milliseconds to wait between each send
, cooldown      ? "10"    # number of minutes to wait for cooldown
, addGenerators ? "6"     # using more than one generator increases the load for stress-tests
, edgeNodes     ? "0"     # the number of edge nodes (wallets)
                          # must be a multiple of 10
, walletsDeployment ? "edgenodes-cluster"  # edge nodes' deployment
, corePolicy    ? "policy_core.yaml"       # policy file for core nodes
, relayPolicy   ? "policy_relay.yaml"      # policy file for relay nodes
}@args:

with import <nixpkgs> {};
writeScriptBin "run-bench.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  # set policy files to be used
  awk '$2=="cfg.enablePolicies" {$1="    (optionalString"; $9="\"''${./../benchmarks/${corePolicy}}\"" ; $11="\"''${./../benchmarks/${relayPolicy}}\")))"} 1' ../modules/cardano-service.nix  > ../modules/cardano-service.nix.new
  mv ../modules/cardano-service.nix.new ../modules/cardano-service.nix

  CLUSTERNAME=`grep name: ../config.yaml | awk '{print $2}'`

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
  TXS_PER_THREAD=${txsPerThread}   # number of transactions that each thread should send
  CONC=${conc}                     # number of threads
  DELAY=${delay}                   # number of milliseconds to wait between each send
  COOLDOWN=${cooldown}             # number of minutes to wait for cooldown
  ADDGENERATORS=${addGenerators}   # using more than one generator might help increase the
                                   # load for stress-tests
  WALLETS_NODES=$((${edgeNodes} / 10))
                                   # each deployment node runs 10 wallets
  WALLETS_DEPLOYMENT=${walletsDeployment}
                                   # edge nodes can be added for further investigation


  export NIXOPS_DEPLOYMENT=''${CLUSTERNAME}

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  TOPOLOGY=`grep topology: ../config.yaml | awk '{print $2}'`
  CONFIGURATIONYAML=`nix-instantiate -E '(import ./. {}).cardano-sl.src + "/configuration.yaml"' --eval`

  # build needed tools
  nix-build -A cardano-sl-auxx -o auxx         # transaction generator

  # clear history of edge nodes
  if [ -n "$WALLETS_DEPLOYMENT" ]; then
    export NIXOPS_DEPLOYMENT=''${WALLETS_DEPLOYMENT}
    nixops ssh-for-each 'for x in cardano-node-{1..10}; do systemctl stop $x ;done'
    nixops ssh-for-each 'rm -rf /home/cardano-node-*/state-wallet-override'
    export NIXOPS_DEPLOYMENT=''${CLUSTERNAME}
  fi

  # re-start the cluster
  $IO -C .. stop wipe-node-dbs --confirm wipe-journals
  $IO -C .. deploy -t ''${START_WAIT_TIME}
  SYSTEMSTART=`grep -A 2 systemStart ../config.yaml | grep contents | awk '{print $2}'`

  # new wallets
  if [ ''${WALLETS_NODES} -gt "0" ]; then
    WALLETS2RELAYS=`nixops info --no-eval --plain | grep 'r-a-1' | awk 'NF>=2 {print $(NF-1)}'`
    sed -i 's/addr: \([0-9.]*\)/addr: '"''${WALLETS2RELAYS}"'/' ../topology-edgenode.yaml
    export NIXOPS_DEPLOYMENT=''${WALLETS_DEPLOYMENT}
    nixops set-args --arg systemStart ''${SYSTEMSTART}  --arg nodes ''${WALLETS_NODES}
    nixops deploy
    export NIXOPS_DEPLOYMENT=''${CLUSTERNAME}
  fi

  # get IP addresses of relays
  # privileged relays (connected directly to the core nodes)
  PRIV_RELAYS=`nixops info | grep 'r-[abc]-[0-9] ' | awk 'NF>=2 {print $(NF-1)}' | sed 's/\([0-9.]*\)/  --peer=\1:3000/'`
  # unprivileged relays (publicly visible)
  UNPRIV_RELAYS=`nixops info | grep 'u-[abc]-[0-9] ' | awk 'NF>=2 {print $(NF-1)}' | sed 's/\([0-9.]*\)/  --peer=\1:3000/'`

  # where trx are sent to
  TRX2RELAYS=''${UNPRIV_RELAYS}
  #TRX2RELAYS=''${PRIV_RELAYS}

  # wait for the cluster to be available
  sleep ''${START_WAIT_TIME}m

  export GENESIS_TXS_PER_THREAD=$TXS_PER_THREAD

  # if 50.000 genesis addresses are not adequate
  # some addresses will send coins back to themselves.
  if [ $((TXS_PER_THREAD * CONC * (ADDGENERATORS + 1))) -gt "50000" ]; then
    export GENESIS_TXS_PER_THREAD=$((50000 / CONC / (ADDGENERATORS + 1)))
  fi

  for n in $(seq 1 $ADDGENERATORS); do

      export AUXX_START_AT=$((GENESIS_TXS_PER_THREAD * CONC * n))

      ./auxx/bin/cardano-auxx \
          --db-path wdb''${n} \
          --log-config ../static/txgen-logging.yaml --logs-prefix experiments/txgen/txgen-''${n}  \
          --configuration-file ''${CONFIGURATIONYAML} \
          --configuration-key bench \
          ''${TRX2RELAYS} \
          --system-start ''${SYSTEMSTART} \
          --mode with-config \
          cmd --commands "send-to-all-genesis $GENESIS_TXS_PER_THREAD $TXS_PER_THREAD $CONC $DELAY ./tps-sent-''${n}.csv" +RTS -s -N1 -RTS > auxx-''${n}.log 2>&1 &

      auxxpids[n]=$!

      sleep 1
      ./scripts/record-stats.sh -pid ''${auxxpids[n]} > auxx-''${n}-ts.log &
      recorderpids[n]=$!
  done

  export AUXX_START_AT=0

  ./auxx/bin/cardano-auxx \
      --db-path wdb \
      --log-config ../static/txgen-logging.yaml --logs-prefix experiments/txgen/txgen  \
      --configuration-file ''${CONFIGURATIONYAML} \
      --configuration-key bench \
      ''${TRX2RELAYS} \
      --system-start ''${SYSTEMSTART} \
      --mode with-config \
      cmd --commands "send-to-all-genesis $GENESIS_TXS_PER_THREAD $TXS_PER_THREAD $CONC $DELAY ./tps-sent.csv" +RTS -s -N1 -RTS > auxx-0.log 2>&1 &

  auxxpids[0]=$!

  sleep 3
 ./scripts/record-stats.sh -pid ''${auxxpids[0]} > auxx-0-ts.log &
  recorderpids[0]=$!

  echo "All the auxx processes: ''${auxxpids[*]}"
  echo "All the recorder processes: ''${recorderpids[*]}"

  sleep 3

  # wait for auxx to finish
  wait ''${auxxpids[0]}

  sleep ''${COOLDOWN}m

  #for P in ''${recorderpids[*]}; do
  #  kill $P
  #done

  # stop edge nodes
  if [ ''${WALLETS_NODES} -gt "0" ]; then
    export NIXOPS_DEPLOYMENT=''${WALLETS_DEPLOYMENT}
    nixops ssh-for-each 'for x in cardano-node-{1..10}; do systemctl stop $x ;done'
    # nixops ssh-for-each 'rm -rf /home/cardano-node-*/state-wallet-override'
    export NIXOPS_DEPLOYMENT=''${CLUSTERNAME}
  fi

  # save arguments with which nix-script was called
  cat <<EOF >run-bench-args.txt
 ${toString (lib.mapAttrsToList (name: value: name + "=" + value + "\n") args)}
EOF

  $(nix-build ./scripts/collect-data.nix              \
  --argstr coreNodes         ${coreNodes}             \
  --argstr startWaitTime     ${startWaitTime}         \
  --argstr txsPerThread      ${txsPerThread}          \
  --argstr conc              ${conc}                  \
  --argstr delay             ${delay}                 \
  --argstr cooldown          ${cooldown}              \
  --argstr addGenerators     ${addGenerators}         \
  --argstr edgeNodes         ${edgeNodes}             \
  --argstr corePolicy        ${corePolicy}            \
  --argstr relayPolicy       ${relayPolicy}           \
  )/bin/collect-data.sh

  nixops ssh-for-each 'systemctl stop cardano-node-recorder; systemctl stop cardano-node'

''
