#!/usr/bin/env bash

set -e
YAML_FILE=edgenodes-testnet.yaml
CLUSTERNAME=policy-experiments
DELAY_MINUTES=10
export NIXOPS_DEPLOYMENT=${CLUSTERNAME}
CMD="./io-new -c $YAML_FILE"

IS_RUNNING=`nixops list | grep ${CLUSTERNAME} | wc -l`
if [ "$IS_RUNNING" -ne "0" ]; then
echo "Stopping old cluster"

$CMD destroy
$CMD delete
fi

$CMD create
echo "Running the cluster is ${DELAY_MINUTES} from now..."
$CMD deploy -t 10
