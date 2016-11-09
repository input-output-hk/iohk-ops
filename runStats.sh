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

while [[ "$#" -ne 0 ]]; do
  case "$1" in
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
   ./redeployHard.sh
   ./update-result-dir.sh
   echo "Pausing for 5 mins"
   sleep 300s
fi

cd "$rundir"

mkdir conf
cp -R "$dir/nixops.nix" "$dir/srk-nixpkgs" conf

echo "Running txgen..."
cardano-tx-generator -d 10 -t 1 -t 5 -t 10 -t 20 -t 30 -t 50 -t 60 -t 70 -t 80 -t 90 -t 100 -t 110 -t 120 -t 130 \
       	-k 600000 -i 0 --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=
exit_ $?

echo "Pausing for 100s"
sleep 100s

echo "Running stats collector..."

# Generate collector.yaml
{
  echo "nodes:"
  nixops info -d cardano --plain \
    | awk '{ print $7 }' | grep -vE '^\s*$' \
    | while read l; do
         echo "- [$l, 3000]"
      done
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
