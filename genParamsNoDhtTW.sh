#!/usr/bin/env bash

# This script is for hack to temporarly launch setup on 20 instances with no dht

nixops info -d cardano2 --plain | grep -v obsolete | awk '{ print $1" "$7 }' | grep -E '^node[0-9]+\s'| sed -r 's/^node([0-9]+)\s/\1 /'| { j=0; while read -a r; do
  i=${r[0]}
  ip=${r[1]}
  echo "--peer $ip:3055";
  j=$((j+1))
done
}
