#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git jq

set -euo pipefail

source scripts/set_nixpath.sh

cardano_rev="$(jq -r .rev < cardano-sl-src.json)"
explorer_path="$(nix-build -A cardano-sl-explorer-static default.nix)"

echo "Building explorer frontend from 'cardano-sl' repository revision:  ${cardano_rev}".

if [ ! -d "cardano-sl" ]; then
  git clone https://github.com/input-output-hk/cardano-sl.git
fi

pushd cardano-sl
  git fetch
  git checkout "${cardano_rev}"
  EXPLORER_EXECUTABLE="${explorer_path}"/bin/cardano-explorer-hs2purs ./explorer/frontend/scripts/build.sh
popd

echo "Built explorer frontend from 'cardano-sl' repository revision:  ${cardano_rev}".
echo "The output store path is:  ${explorer_path}"
