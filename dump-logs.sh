#!/usr/bin/env bash

d=`date +%F_%H%M%S`
logs_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/logs/$d
mkdir -p "$logs_dir"
cd "$logs_dir"

function get_names {
  nixops info --plain | awk '{print $1}' | grep node
}

for n in `get_names`; do
  nixops scp --from $n /var/lib/cardano-node/cardano-node.log $n.log 1>&2 &
done

nixops info > layout.txt

cd ..

rm latest
ln -s `pwd`/$d latest

wait

tar -czf $d.tgz $d

