#!/usr/bin/env bash

function exit_ {
   e=$1
   if [[ $e -ne 0 ]]; then
     echo ".. failed with code $e"
     exit $e
   fi
}

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$dir"

reboot=false
deployment=cardano
while [[ "$#" -ne 0 ]]; do
  case "$1" in
    -d)
       deployment=$2
       shift
       ;;
    --reboot)
       reboot=true
       ;;
    *)
       break
       ;;
  esac
  shift
done

function check_status {

nixops info --plain | grep -v obsolete | awk '{print $1" "$7}' | grep node | { while read -a r; do
   ip=${r[1]}
   name=${r[0]}
   { nixops ssh $name -o ConnectTimeout=2 echo -n ''
     e=$?
     if [[ $e -ne 0 ]]; then
       echo "$name ($ip) failed" >&2
       echo -n "$name "
     fi
   } &
done; wait; }

}

failed_names=`check_status`
echo "Failed names: $failed_names" >&2
if $reboot && [[ "$failed_names" != "" ]]; then
   echo "Rebooting $failed_names..." >&2
   nixops reboot --include $failed_names
   #echo "Nodes $failed_names are dead. @georgeee do smth"
   #exit 1
   failed_names=`check_status`
   echo "Failed names: $failed_names" >&2
fi
echo $failed_names
if [[ "$failed_names" != "" ]]; then
   exit 1
fi
