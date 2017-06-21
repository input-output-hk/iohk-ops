#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git jq

set -euo pipefail

source scripts/set_nixpath.sh

if [ ! -d "cardano-sl-explorer" ]; then
  git clone https://github.com/input-output-hk/cardano-sl-explorer.git
fi

pushd cardano-sl-explorer
  git fetch
  git checkout "$(nix-instantiate --eval ../default.nix -A cardano-sl-explorer-static.src.rev|tr -d '"')"
  pushd frontend
    EXPLORER_NIX_FILE=../default.nix ./scripts/build.sh
  popd
popd
