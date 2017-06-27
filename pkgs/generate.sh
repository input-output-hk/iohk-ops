#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p pkgs.cabal2nix pkgs.nix-prefetch-scripts pkgs.coreutils pkgs.cabal-install
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/c0b1e8a5fb174cd405dcca9f7fec275714ad9f4b.tar.gz

set -xe

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

# Generate stack2nix Nix package
cabal2nix \
  --no-check \
  --revision 3f2d4d31b1936dda71ce9a3be145c6407685045e \
  https://github.com/input-output-hk/stack2nix.git > $scriptDir/stack2nix.nix

# Build stack2nix Nix package
nix-build -E "with import <nixpkgs> {}; haskell.packages.ghc802.callPackage ${scriptDir}/stack2nix.nix {}" -o $scriptDir/stack2nix

# Generate explorer until it's merged with cardano-sl repo
cabal2nix \
  --no-check \
  --revision 1bb82fd04b5af51580b591776c75cebf681cd25d \
  https://github.com/input-output-hk/cardano-sl-explorer.git > $scriptDir/cardano-sl-explorer.nix

# Generate cardano-sl package set
$scriptDir/stack2nix/bin/stack2nix \
  --revision b3fe5e64c06f3931dff4e317504e5c6f37fbda50 \
  https://github.com/input-output-hk/cardano-sl.git > $scriptDir/default.nix
