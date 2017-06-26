
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
  --revision $(jq .rev < ../cardano-sl-explorer-src.json -r) \
  https://github.com/input-output-hk/cardano-sl-explorer.git > $scriptDir/cardano-sl-explorer.nix

# Generate cardano-sl package set
runInShell $scriptDir/stack2nix/bin/stack2nix \
  --revision $(jq .rev < ../cardano-sl-src.json -r) \
  https://github.com/input-output-hk/cardano-sl.git > $scriptDir/default.nix

# Generate iohk-ops expression for Hydra
# Manual build with: nix-build --no-build-output --cores 0 -E "with import <nixpkgs> {}; haskell.packages.ghc802.callPackage iohk/default.nix {}"
runInShell cabal2nix $scriptDir/.. > $scriptDir/../iohk/default.nix
