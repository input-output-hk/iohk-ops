#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/763e21e982370f67c126f92a1113ea949db3b6e0.tar.gz

if [ ! -d "cardano-sl-explorer" ]; then
  git clone https://github.com/input-output-hk/cardano-sl-explorer.git
fi

pushd cardano-sl-explorer
# checkout the revision used by Nix
git checkout $(nix-instantiate --eval ../default.nix -A cardano-sl-explorer-static.src.rev|tr -d '"')
pushd frontend
EXPLORER_NIX_FILE=../default.nix ./scripts/build.sh
popd
popd
