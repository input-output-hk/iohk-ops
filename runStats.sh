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


th=5
noRedeploy=false
noTxgen=false
restart_=false
experiment="-d 120 -t 50"

stabilization_pause=150

while [[ "$#" -ne 0 ]]; do
  case "$1" in
    --no-txgen)
       noTxgen=true
       ;;
    --no-redeploy)
       noRedeploy=true
       ;;
    --restart)
       restart_=true
       ;;
    -e | --experiment)
       experiment="$2"
       shift
       ;;
    *)
       break
       ;;
  esac
  shift
done

echo "Txgen params: $experiment"

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
   if $restart_; then
     ./redeployHard.sh restart
   else
     ./redeployHard.sh
   fi
   exit_ $?
   ./update-result-dir.sh
   echo "Pausing for 5 mins"
   sleep 300s
fi

cd "$rundir"

mkdir conf
cp -R "$dir/nixops.nix" "$dir/srk-nixpkgs" conf

echo "Running txgen..."
cardano-tx-generator $experiment \
       	-k 6000 -i 0 \
	`"$dir/genParamsNoDht.sh"` \
	--explicit-initial \
	2>&1 | tee txgen.log
	#--peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=
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
  n=`nixops info -d cardano --plain | grep -v obsolete | wc -l`
  th2=$(($n/8))
  if [[ $th -lt $th2 ]]; then
     th=$th2
  fi
  nixops info -d cardano --plain | grep -v obsolete\
    | grep -vE "$failedE" \
    | awk '{ print $7 }' | grep -vE '^\s*$' \
    | while read l; do
         echo "- [$l, 3000]"
      done | shuf | head -"$th"
} > collector.yaml
mkdir stats

cardano-stats-collector --output-dir stats --config collector.yaml --ssh-passwd 123123123123 --interval 100500 \
	2>&1 | tee collector.log
exit_ $?

echo "Pausing for ${stabilization_pause}s (waiting for blocks' stabilization)"
sleep ${stabilization_pause}s

echo "Dumping logs..."

"$dir/dump-logs.sh"

cp -av "$dir/logs/latest" logs

cd ..

echo "Creating $(pwd)/$name.tgz"

tar -czf $name.tgz $name -h
