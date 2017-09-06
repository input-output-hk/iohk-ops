
update_NIX_PATH() {
  if [ "x$NIX_PATH_LOCKED" == "x1" ]; then
    return
  fi
  local scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  nix-build "${scriptDir}/../lib.nix" -A fetchNixPkgs -o "${scriptDir}/iohk-nixpkgs"
  export NIX_PATH="nixpkgs=${scriptDir}/iohk-nixpkgs"
  export NIX_PATH_LOCKED=1
}
update_NIX_PATH
