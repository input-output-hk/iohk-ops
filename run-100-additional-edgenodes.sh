#!/usr/bin/env bash

set -e
YAML_FILE=edgenodes-only-testnet.yaml
CLUSTERNAME=policy-experiments-100-additional-edgenodes-1
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
$CMD modify
echo "Running the cluster is ${DELAY_MINUTES} from now..."
$CMD deploy -t 10
