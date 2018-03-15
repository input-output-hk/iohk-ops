#!/usr/bin/env bash
update_NIX_PATH() {
  local scriptDir
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  NIX_PATH="nixpkgs=$(nix-build "${scriptDir}/../fetch-nixpkgs.nix" -o nixpkgs)"
  export NIX_PATH
}
update_NIX_PATH
