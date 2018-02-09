#!/usr/bin/env bash

set -e        # exit on error
set -o xtrace # print commands

CLUSTERNAME=benchmarks110
CORENODES=6 # the number of core nodes.
# When changing the number of core nodes, you need to:
# - adjust the topology file
# - adjust lib/configuration.yaml in cardano-sl
#   change richmen in the bench section
#   commit the change, and push
#   use io set-rev cardanosl <COMMIT>, to use the new commit
#   re-generate the keys, by running
#     io -v new -t topology-staging.yaml -k bench benchmarks110 nodes
#     This step will take some time
START_WAIT_TIME=10 # how many minutes to wait for the cluster to start
                   # before starting the transaction generator

TIME=400       # number of transactions that each thread should send
CONC=6          # number of threads
DELAY=50        # number of milliseconds to wait between each send
SENDMODE='send-random'
COOLDOWN=10      # number of minutes to wait for cooldown
ADDGENERATORS=1 # using more than one generator might help increase the
                # load for stress-tests

export NIXOPS_DEPLOYMENT=${CLUSTERNAME}

IO=$(nix-build -A iohk-ops)/bin/iohk-ops

TOPOLOGY=`grep topology: config.yaml | awk '{print $2}'`
CONFIGURATIONYAML=`nix-instantiate -E '(import ./. {}).cardano-sl.src + "/configuration.yaml"' --eval`

# build needed tools
nix-build -A cardano-sl-auxx -o auxx         # transaction generator
nix-build -A cardano-sl-tools -o post-mortem # for cardano-post-mortem

# # re-start the cluster
$IO stop
$IO deploy -t ${START_WAIT_TIME}
SYSTEMSTART=`grep -A 2 systemStart config.yaml | grep contents | awk '{print $2}'`

# get IP addresses of relays
# privileged relays (connected directly to the core nodes)
PRIV_RELAYS=`nixops info | grep 'r-[abc]-[0-9] ' | awk 'NF>=2 {print $(NF-1)}' | sed 's/\([0-9.]*\)/  --peer=\1:3000/'`
# unprivileged relays (publicly visible)
UNPRIV_RELAYS=`nixops info | grep 'u-[abc]-[0-9] ' | awk 'NF>=2 {print $(NF-1)}' | sed 's/\([0-9.]*\)/  --peer=\1:3000/'`

# wait for the cluster to be available
sleep ${START_WAIT_TIME}m

for n in $(seq 1 $ADDGENERATORS); do

    #export AUXX_START_AT=${OFFSET[n]}
    export AUXX_START_AT=$((TIME * CONC * n))

    ./auxx/bin/cardano-auxx \
    	--db-path wdb$n \
	--log-config static/txgen-logging.yaml --logs-prefix experiments/txgen/txgen-$n  \
	--configuration-file ${CONFIGURATIONYAML} \
	--configuration-key bench \
	${UNPRIV_RELAYS} \
	--system-start $SYSTEMSTART \
        --mode with-config \
	cmd --commands "send-to-all-genesis $TIME $CONC $DELAY $SENDMODE ./tps-sent-$n.csv" +RTS -s -N1 -RTS >/dev/null 2>&1 &

done

export AUXX_START_AT=0

./auxx/bin/cardano-auxx \
    --db-path wdb \
    --log-config static/txgen-logging.yaml --logs-prefix experiments/txgen/txgen  \
    --configuration-file ${CONFIGURATIONYAML} \
    --configuration-key bench \
    ${UNPRIV_RELAYS} \
    --system-start $SYSTEMSTART \
    --mode with-config \
    cmd --commands "send-to-all-genesis $TIME $CONC $DELAY $SENDMODE ./tps-sent.csv" +RTS -s -N1 -RTS # >/dev/null 2>&1

sleep ${COOLDOWN}m

$IO dumplogs nodes

# timestamp identifying this run
LAST=`ls experiments/ | grep 20 | sort | tail -1`
LOGDIR="experiments/${LAST}"
# commit of cardano-sl
COMMIT=`grep rev cardano-sl-src.json | awk '{print $2}' | cut -c 2- | cut -c -7`
# cluster topology file
TOPOLOGY=`grep topology: config.yaml | awk '{print $2}'`

# archive settings and topology file
echo "commit=${COMMIT}, sendmode=${SENDMODE}, time=${TIME}, conc=${CONC}, delay=${DELAY}, generators=$((ADDGENERATORS + 1)), systemstart=${SYSTEMSTART}" > ${LOGDIR}/bench-settings
cp $TOPOLOGY ${LOGDIR}/

# parse slot duration and start time from tx generator output
SLOTDURATION=`grep -oP '(?<=slotDuration=)[0-9]+' tps-sent.csv`
STARTTIME=`grep -oP '(?<=startTime=)[0-9]+' tps-sent.csv`

# run post-mortem tool to parse json logs from nodes
./post-mortem/bin/cardano-post-mortem  overview 0.05 ${LOGDIR}

# assemble csv file from tx generator and node logs
TPSFILE="run-${LAST}.csv"
echo "time,txCount,txType,slotDuration,conc,sendMode,clustersize,startTime,commit,node,run,time,delay" > ${TPSFILE}
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
    tps-sent.csv >> ${TPSFILE}
mv tps-sent.csv ${LOGDIR}
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
	tps-sent-$n.csv >> ${TPSFILE}
    mv tps-sent-$n.csv ${LOGDIR}
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
    csv_${LAST}.csv >> ${TPSFILE}
mv csv_${LAST}.csv ${LOGDIR}

# move files
mv ${TPSFILE} ${LOGDIR}
mv experiments/txgen ${LOGDIR}
mv report_${LAST}.txt ${LOGDIR}
mv times.png ${LOGDIR}/times.svg

echo "--- Benchmarks finished. Find the results at"
echo "    ${PWD}/${LOGDIR}/${TPSFILE}"
echo "    ${PWD}/${LOGDIR}/report_${LAST}.txt"
echo "    ${LOGDIR}"
