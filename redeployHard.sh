#echo "stopping 0"
#nixops ssh node0-coordinator -- systemctl kill cardano-node
#for i in {1..100}; do echo "stopping $i"; nixops ssh node$i -- systemctl kill cardano-node; done

#Hack, but can't imagine better
#yes_file=/tmp/pos-prototype-deployment-yes-file.txt
#yes | head -100 > $yes_file

cmd=$1

if [[ "$cmd" == "" ]]; then
  cmd=redeploy
fi

deploy_args=$2

shift
shift

batch=34
pause=450
pause2=100
node_count=`nixops info --plain | grep node | wc -l`
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
   local i=$((batch_cnt-2))
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
     echo "Pausing for $pause_ sec"
     sleep ${pause_}s
     i=$((i-1))
   done
}

function stop_node {
  nixops stop --include $@
}

function deploy_node {
  nixops deploy -d cardano -I nixpkgs=~/nixpkgs --show-trace $deploy_args --include $@
}

case "$cmd" in
   stop | redeploy )
     echo "Stopping via SSH"
     k=0
     while [[ $k -lt $node_count ]]; do
        nixops ssh node$k systemctl stop cardano-node &
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
