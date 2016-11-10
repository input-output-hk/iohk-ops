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


noRedeploy=false
noTxgen=false

while [[ "$#" -ne 0 ]]; do
  case "$1" in
    --no-txgen)
       noTxgen=true
       ;;
    --no-redeploy)
       noRedeploy=true
       ;;
    *)
       break
       ;;
  esac
  shift
done

if [[ "$1" == "" ]]; then
  echo "No name provided"
  exit 2
fi

name=$1
rundir="$dir/../stat-runs/$1"

echo "Creating dir $rundir..."
mkdir "$rundir"
exit_ $?

if ! $noRedeploy; then
   echo "Checking nodes..."
   ./checkStatus.sh --reboot
   exit_ $?
   ./redeployHard.sh
   exit_ $?
   ./update-result-dir.sh
   echo "Pausing for 5 mins"
   sleep 300s
fi

cd "$rundir"

mkdir conf
cp -R "$dir/nixops.nix" "$dir/srk-nixpkgs" conf

echo "Running txgen..."
cardano-tx-generator -d 750 -t 40 \
       	-k 600000 -i 0 --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=
exit_ $?

failed=`$dir/checkStatus.sh`
failedE=`echo "$failed" | sed 's/^\s*//' | sed 's/\s*$//' | sed 's/\s\s*/\|/g'`
if [[ "$failedE" == "" ]]; then
   failedE="abracadabrablabla"
fi
  
echo "Running stats collector..."

# Generate collector.yaml
{
  echo "nodes:"
  n=`nixops info -d cardano --plain | wc -l`
  th2=12
  if [[ $th -lt $th2 ]]; then
     th=$th2
  fi
  nixops info -d cardano --plain \
    | grep -vE "$failedE" \
    | awk '{ print $7 }' | grep -vE '^\s*$' \
    | while read l; do
         echo "- [$l, 3000]"
      done | shuf | head -"$th"
} > collector.yaml
mkdir stats

cardano-stats-collector --output-dir stats --config collector.yaml --ssh-passwd 123123123123 --interval 100500
exit_ $?

echo "Dumping logs..."

"$dir/dump-logs.sh"

cp -av "$dir/logs/latest" logs

cd ..

echo "Creating $(pwd)/$name.tgz"

tar -czf $name.tgz $name -h
