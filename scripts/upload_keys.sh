for i in {0..13}; do
 nixops scp -d csl-testnet --to node$i ./nodes/key$((i+1)) /var/lib/cardano-node/key$((i+1)).sk
 nixops ssh -d csl-testnet node$i chown cardano-node:cardano-node /var/lib/cardano-node/key$((i+1)).sk
done

