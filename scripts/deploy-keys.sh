#!/bin/sh

set +x

cp node0.key mainnet/keys/node0.key   # IOHK 0
cp node1.key mainnet/keys/node1.key   # IOHK 1
cp node6.key mainnet/keys/node2.key   # IOHK 2

cp node2.key mainnet/keys/node3.key   # Emurgo 0
cp node3.key mainnet/keys/node4.key   # Emurgo 1

cp node4.key mainnet/keys/node5.key   # CF 0
cp node5.key mainnet/keys/node6.key   # CF 1
