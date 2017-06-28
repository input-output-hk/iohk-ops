{ pkgs ? (import <nixpkgs> {}), supportedSystems ? [ "x86_64-linux" ] }:

with pkgs;

let
  iohkpkgs = import ./../default.nix {};
in rec {
  inherit (iohkpkgs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static cardano-sl;
  stack2nix = iohkpkgs.callPackage ./../pkgs/stack2nix.nix {};
  tests = import ./../tests { inherit pkgs supportedSystems; };
}
