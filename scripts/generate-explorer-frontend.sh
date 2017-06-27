#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/9b948ea439ddbaa26740ce35543e7e35d2aa6d18.tar.gz

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
