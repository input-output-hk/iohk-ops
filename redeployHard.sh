#echo "stopping 0"
#nixops ssh node0-coordinator -- systemctl kill cardano-node
#for i in {1..100}; do echo "stopping $i"; nixops ssh node$i -- systemctl kill cardano-node; done
yes | nixops stop 
nixops deploy -d cardano -I nixpkgs=~/nixpkgs --show-trace $1
