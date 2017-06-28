#!/bin/sh

function runInShell {
  nix-shell -j 4 -p cabal2nix nix-prefetch-scripts coreutils cabal-install stack --run "$*"
}

set -xe

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../scripts/set_nixpath.sh

# Generate stack2nix Nix package
runInShell cabal2nix \
  --no-check \
  --revision 8f087b92f83be078e8cfe86fb243121dca4601ba \
  https://github.com/input-output-hk/stack2nix.git > $scriptDir/stack2nix.nix

# Build stack2nix Nix package
nix-build -E "with import <nixpkgs> {}; haskell.packages.ghc802.callPackage ${scriptDir}/stack2nix.nix {}" -o $scriptDir/stack2nix

# Generate explorer until it's merged with cardano-sl repo
runInShell cabal2nix \
  --no-check \
  --revision 982ac2dd3efeca6fd7ebc1df11e9d1bf628c1700 \
  https://github.com/input-output-hk/cardano-sl-explorer.git > $scriptDir/cardano-sl-explorer.nix

# Generate cardano-sl package set
runInShell $scriptDir/stack2nix/bin/stack2nix \
  --revision 4bcf1e6b601d531f753ebdb6aec23e19d4b08e6c \
  https://github.com/input-output-hk/cardano-sl.git > $scriptDir/default.nix
