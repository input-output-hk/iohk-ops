#!/usr/bin/env bash

# This script is for hack to temporarly launch setup on 20 instances with no dht

function dht_key {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
  	i2="0$i"
  fi
  
  echo "MHdtsP-oPf7UWly"$i2"7QuXnLK5RD="
}

nixops info -d cardano --plain | grep -v obsolete | awk '{ print $1" "$7 }' | grep -E '^node[0-9]+\s'| sed -r 's/^node([0-9]+)\s/\1 /'| { j=0; while read -a r; do
  i=${r[0]}
  ip=${r[1]}
  echo "--peer $ip:3000/`dht_key $j`";
  j=$((j+1))
done
}
