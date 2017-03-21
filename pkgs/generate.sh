#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p pkgs.cabal2nix
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/464c79ea9f929d1237dbc2df878eedad91767a72.tar.gz

set -xe

cabal2nix https://github.com/serokell/universum --no-check --revision 9edb5d374ec1fb6bb873dacc22a013f1eacd9c67 > universum.nix 
cabal2nix https://github.com/serokell/serokell-util.git --revision af79ca0c42b8fd23c499096f91f649edfb0862f1 > serokell-util.nix
cabal2nix https://github.com/serokell/acid-state.git --revision 95fce1dbada62020a0b2d6aa2dd7e88eadd7214b > acid-state.nix
cabal2nix https://github.com/serokell/log-warper.git --revision b3b2d4d5afa2c22f3aff10b4cceab3914c6bc0e5 > log-warper.nix
cabal2nix https://github.com/serokell/kademlia.git --no-check --revision bf65ac0cd50d2ccd7ef6507f0d71786c4bd10ae1 > kademlia.nix
cabal2nix https://github.com/serokell/rocksdb-haskell.git --revision 4dfd8d61263d78a91168e86e8005eb9b7069389e > rocksdb-haskell.nix
cabal2nix https://github.com/serokell/time-warp-nt.git --no-check --revision 2943e7814b9f78b9ef68c9bdc96820ed32a0fdf3 > time-warp-nt.nix

cabal2nix https://github.com/input-output-hk/cardano-sl.git --no-check --revision c10bcd8f19d538c58251e33a994e3246aeda891c > cardano-sl.nix
cabal2nix https://github.com/input-output-hk/cardano-report-server.git --revision 424e4ecacdf038a01542025dd1296bd272ce770d > cardano-report-server.nix
cabal2nix https://github.com/input-output-hk/plutus-prototype.git --revision e2e2711e6978002279b4d7c49cab1aff47a2fd43 > plutus-prototype.nix

cabal2nix https://github.com/thoughtpolice/hs-ed25519.git --revision da4247b5b3420120e20451e6a252e2a2ca15b43c > ed25519.nix
cabal2nix https://github.com/avieth/network-transport.git --no-check --revision f2321a103f53f51d36c99383132e3ffa3ef1c401 > network-transport.nix
cabal2nix https://github.com/avieth/network-transport-tcp.git --no-check --revision 1739cc6d5c73257201e5551088f4ba56d5ede15c > network-transport-tcp.nix
cabal2nix https://github.com/ndmitchell/derive.git --no-check --revision 9b564c23543d92757168581beb832f4dc0db223b > derive.nix
cabal2nix https://github.com/input-output-hk/cardano-crypto --no-check --revision 838b064d8a59286142aa2fe14434fe7601896ddb > cardano-crypto.nix
cabal2nix https://github.com/haskell-crypto/cryptonite.git --no-check --revision 6440a7ebab7d85612e47099017bee0da6339af05 > cryptonite.nix
cabal2nix https://github.com/input-output-hk/cardano-sl-explorer.git --no-check --revision 349cf68b7453430f649880eb4dabd2ed0b55694f > cardano-sl-explorer.nix
