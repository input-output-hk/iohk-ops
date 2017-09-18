
update_NIX_PATH() {
  local scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  nix-build "${scriptDir}/../lib.nix" -A fetchNixPkgs -o "${scriptDir}/iohk-nixpkgs"
  export NIX_PATH="nixpkgs=${scriptDir}/iohk-nixpkgs"
}
update_NIX_PATH
