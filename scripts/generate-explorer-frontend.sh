#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/7648f528de9917933bc104359c9a507c6622925c.tar.gz

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
