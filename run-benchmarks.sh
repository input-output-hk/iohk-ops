#!/usr/bin/env bash

set -e        # exit on error
set -o xtrace # print commands

CLUSTERNAME=edgenodes-scaling
SENDMODE='send-random'
TIME=100    # how many seconds should the generator send transactions
CONC=5      # number of threads in the generator
DELAY=0  # number of ms each thread waits between transactions
COOLDOWN=10 # number of slots for cooldown, i.e., not sending
	    # transactions, and allowing the mempools to empty

export NIXOPS_DEPLOYMENT=${CLUSTERNAME}

## TODO: -DCONFIG=benchmark somewhere within the nix setup
./io-new stop
./io-new deploy -t 5
nixops reboot --include edgenode-1 # edgenode-2 edgenode-3 edgenode-4 edgenode-5 edgenode-6 edgenode-7 edgenode-8 edgenode-9 edgenode-10
nix-build --no-build-output --cores 0 -A cardano-sl-lwallet-static -o lwallet
nix-build --no-build-output --cores 0 -A cardano-post-mortem-static -o post-mortem

# Take the number of core nodes from the config file instead.  It must be updated there anyway.
CORENODES=`grep genesisN config.nix | sed 's/\s*genesisN\s*=\s*\([0-9]*\)\s*;\s*/\1/'`
echo "Starting benchmarks for cluster ${CLUSTERNAME} of size ${CORENODES}"

## get the systemStart from the config file
SYSTEMSTART=`grep -A 2 systemStart edgenodes-testnet.yaml | grep contents | awk '{print $2}'`
TOPOLOGY=`grep topology: edgenodes-testnet.yaml | awk '{print $2}'`
# RELAY_IP=`nixops info | grep 'relay-1 ' | awk 'NF>=2 {print $(NF-1)}'`
RELAYS=`nixops info | grep 'r-[abc]-[0-9] ' | awk 'NF>=2 {print $(NF-1)}' | sed 's/\([0-9.]*\)/  --peer=\1:3000/'`

sleep 2m

./lwallet/bin/cardano-auxx \
    --db-path wdb \
    --log-config static/csl-logging.yaml --logs-prefix experiments/wallet  \
    --rich-poor-distr "($CORENODES,50000,6000000000,0.99)"  \
    ${RELAYS} \
    --system-start $SYSTEMSTART cmd --commands "send-to-all-genesis $TIME $CONC $DELAY $SENDMODE tps-sent.csv" +RTS -s -N2 -RTS

./io-new dumplogs Nodes -p

LAST=`ls experiments/ | grep 20 | sort | tail -1`
echo $LAST

COMMIT=`grep rev cardano-sl-src.json | awk '{print $2}' | cut -c 2- | cut -c -40`
echo $COMMIT

echo "commit=${commit}, sendmode=${SENDMODE}, time=${TIME}, conc=${CONC}, delay=${DELAY}, systemstart=${SYSTEMSTART}" > experiments/${LAST}/bench-settings
cp $TOPOLOGY experiments/${LAST}


SLOTDURATION=`grep -oP '(?<=slotDuration=)[0-9]+' tps-sent.csv`
STARTTIME=`grep -oP '(?<=startTime=)[0-9]+' tps-sent.csv`

# Record sent transactions from the wallet's output
TPSFILE="tps-final-${LAST}.csv"
echo "time,txCount,txType,slotDuration,conc,sendMode,clustersize,startTime,commit,node,run,time,delay" > $TPSFILE
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

./post-mortem/bin/cardano-post-mortem  overview 0.05 experiments/${LAST}
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
    csv_${LAST}.csv >> $TPSFILE

# move files
mv $TPSFILE tps-sent.csv "experiments/$LAST/"
mv experiments/wallet "experiments/$LAST/"
