#!/usr/bin/env nix-shell

set -e
CLUSTERNAME=edgenodes-scaling
SENDMODE='send-random'
TIME=600    # how many seconds should the generator send transactions
CONC=1      # number of threads in the generator
DELAY=2000  # number of ms each thread waits between transactions
COOLDOWN=10 # number of slots for cooldown, i.e., not sending
	    # transactions, and allowing the mempools to empty

export NIXOPS_DEPLOYMENT=${CLUSTERNAME}

## TODO: -DCONFIG=benchmark somewhere within the nix setup
./io-new do deploy stop start
nix-build --no-build-output --cores 0 -A cardano-sl-lwallet-static -o lwallet

CLUSTERSIZE=`nixops info | grep '| core-[0-9]* ' | wc -l`

echo "Starting benchmarks for cluster ${CLUSTERNAME} of size ${CLUSTERSIZE}"

## TODO: figure out systemStart

./lwallet/bin/cardano-wallet \
    --db-path wdb \
    --log-config static/csl-logging.yaml --logs-prefix experiments/wallet  \
    --rich-poor-distr "($CLUSTERSIZE,50000,6000000000,0.99)"  \
    --topology topology-edgenode-3.yaml
    --system-start 1504117922 cmd --commands "send-to-all-genesis $TIME $CONC $DELAY $COOLDOWN $SENDMODE tps-sent.csv" +RTS -s -RTS

./io-new dumplogs Nodes -p

last=`ls experiments/ | grep 20 | sort | tail -1`
