#!/usr/bin/env bash

deployment_name=$1

if [[ "$deployment_name" == "" ]]; then
  deployment_name=cardano
fi

d=`date +%F_%H%M%S`
logs_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/logs/$d
mkdir -p "$logs_dir"
cd "$logs_dir"

function get_names {
  nixops info -d $deployment_name --plain | grep -v obsolete | awk '{print $1}' | grep node
}

for n in `get_names`; do
  nixops scp -d $deployment_name --from $n /var/lib/cardano-node/cardano-node.log $n.log 1>&2 &
  nixops scp -d $deployment_name --from $n /var/lib/cardano-node/jsonLog.json $n.json 1>&2 &
done

nixops info -d $deployment_name > layout.txt

wait

../../logs-analyze/blocks.sh > blocks.log

cd ..

rm latest
ln -s `pwd`/$d latest

tar -czf $d.tgz $d

