#!/usr/bin/env nix-shell
#! nix-shell -p jq -i bash

function runInShell {
  nix-shell -j 4 -p cabal2nix nix-prefetch-scripts coreutils cabal-install stack --run "$*"
}
function c2n {
  runInShell cabal2nix "$*"
}

set -xe
set -v

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../scripts/set_nixpath.sh

# Build stack2nix Nix package
nix-build ${scriptDir}/.. -A stack2nix -o $scriptDir/stack2nix

c2n --no-check --revision $(jq .rev < "${scriptDir}/engine-io.json") $(jq .url < "${scriptDir}/engine-io.json") --subpath socket-io > "${scriptDir}/socket-io.nix"
c2n --no-check --revision $(jq .rev < "${scriptDir}/engine-io.json") $(jq .url < "${scriptDir}/engine-io.json") --subpath engine-io > "${scriptDir}/engine-io.nix"
c2n --no-check --revision $(jq .rev < "${scriptDir}/engine-io.json") $(jq .url < "${scriptDir}/engine-io.json") --subpath engine-io-wai > "${scriptDir}/engine-io-wai.nix"
pushd "${scriptDir}"
c2n --no-check ../iohk > "${scriptDir}/iohk-ops.nix"
popd

# Generate cardano-sl package set
runInShell $scriptDir/stack2nix/bin/stack2nix \
  --revision $(jq .rev < ${scriptDir}/../cardano-sl-src.json -r) \
  https://github.com/input-output-hk/cardano-sl.git > $scriptDir/default.nix.new
mv $scriptDir/default.nix.new $scriptDir/default.nix

