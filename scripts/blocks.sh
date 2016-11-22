#!/usr/bin/env bash

dir=$1

if [[ "$dir" != "" ]]; then
   cd "logs/$dir"
fi

for i in {0..1000}; do
   if [[ -f node$i.log ]]; then
     grep -n 'Created a new block' node$i.log \
	     | sed -r 's/^([0-9]+):\S*\s.....(\S*)\s(\S*)\s\S*.\s.*$/\2_\3 \1/' \
	| while read l; do
     	    echo "$l $i"	
     done
   fi
done | sort | while read -a l; do
   t=${l[0]}
   n=${l[1]}
   i=${l[2]}
   n2=`tail -n +$((n+1)) node$i.log | grep -nE 'ERROR|WARN|DEBUG|INFO|NOTICE'| head -1 | sed -r 's/^([0-9]*):.*$/\1/'`
   echo -n "[$t] node$i: "
   tail -n +$((n+1)) node$i.log | head -n $((n2-1))
done
