let
  fixedNixpkgs = (import ../lib.nix).fetchNixPkgs;
in { pkgs ? (import fixedNixpkgs {}), supportedSystems ? [ "x86_64-linux" ] }:


let
  iohkpkgs = import ./../default.nix { inherit pkgs; };
in with pkgs; rec {
  inherit (iohkpkgs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static cardano-sl;
  stack2nix = iohkpkgs.callPackage ./../pkgs/stack2nix.nix {};
  tests = import ./../tests { inherit pkgs supportedSystems; };
}
