
echo "cleaning node0"
nixops ssh node0-coordinator "systemctl stop cardano-node && rm -rf /var/lib/cardano-node/* && systemctl start cardano-node" && echo "done 0"&
for i in {1..4}; do 
	echo "cleaning node $i"
	nixops ssh node$i "systemctl stop cardano-node && rm -rf /var/lib/cardano-node/* && systemctl start cardano-node" && echo "done $i"&
done

sleep 30
