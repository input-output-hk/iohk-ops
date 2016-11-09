#echo "stopping 0"
#nixops ssh node0-coordinator -- systemctl kill cardano-node
#for i in {1..100}; do echo "stopping $i"; nixops ssh node$i -- systemctl kill cardano-node; done

#Hack, but can't imagine better
#yes_file=/tmp/pos-prototype-deployment-yes-file.txt
#yes | head -100 > $yes_file

function exit_ {
   e=$1
   if [[ $e -ne 0 ]]; then
     echo ".. failed with code $e"
     exit $e
   fi
}

cmd=$1

if [[ "$cmd" == "" ]]; then
  cmd=redeploy
fi

deployment_name=$2

if [[ "$deployment_name" == "" ]]; then
  deployment_name=cardano
fi

deploy_args=$3

shift
shift

batch=40
pause=500
pause2=100
node_count=`nixops info -d $deployment_name --plain | grep node | wc -l`
batch_cnt=$((node_count/batch))

if [[ $((batch_cnt*batch)) -lt $node_count ]]; then
   batch_cnt=$((batch_cnt+1))
fi

function node_list {
  local i=$1
  local j=$((i*batch))
  while [[ $j -lt $((i*batch+batch)) ]]; do
   echo -n "node$j "
   j=$((j+1))
  done
}

function runBatched {
   local pause_=$3
   local i=$((batch_cnt-1))
   while [[ $i -gt -1 ]]; do
     echo "$2 nodes $((i*batch))..$((i*batch+batch-1))"
     #j=$((i*batch))
     #pids=''
     #while [[ $j -lt $((i*batch+batch)) ]]; do
     #  $1 $j 1>&2 <$yes_file &
     #  pids="$pids $!"
     #  j=$((j+1))
     #done
     #echo "Pids: $pids"
     #wait
     echo $1 `node_list $i`
     yes | $1 `node_list $i`
     if [[ $i == 0 ]]; then
       break;
     fi
     echo "Pausing for $pause_ sec"
     sleep ${pause_}s
     i=$((i-1))
   done
}

function stop_node {
  echo nixops stop -d $deployment_name --include $@ >&2
  nixops stop -d $deployment_name --include $@
  exit_ $?
}

function stop_node_light {
  echo nixops ssh -d $deployment_name node$1 systemctl stop cardano-node >&2
  nixops ssh -d $deployment_name node$1 systemctl stop cardano-node
  exit_ $?
}

function deploy_node {
  echo nixops deploy -d $deployment_name -I nixpkgs=~/nixpkgs --show-trace $deploy_args --include $@ >&2
  local tmp_file=/tmp/tmp_file_`date +%F%H%M%S`.txt
  local e=1
  while [[ $e -ne 0 ]]; do
     nixops deploy -d $deployment_name -I nixpkgs=~/nixpkgs --show-trace $deploy_args --include $@ > $tmp_file
     e=$?
     if [[ $e -eq 0 ]]; then
        e=`grep "error" $tmp_file|wc -l`
     fi
  done
}

case "$cmd" in
   stop | redeploy )
     echo "Stopping via SSH"
     k=0
     while [[ $k -lt $node_count ]]; do
	stop_node_light $k &
	k=$((k+1))
     done
     wait
     #runBatched stop_node "Stopping" $pause
     ;;
esac
if [[ "$cmd" == "redeploy" ]]; then
     echo "Pausing for $pause2 sec"
     sleep ${pause2}s
fi
case "$cmd" in
   redeploy | deploy | start )
     runBatched deploy_node "Deploying" $pause
     ;;
esac
