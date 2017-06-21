{ pkgs' ? (import <nixpkgs> {}), supportedSystems ? [ "x86_64-linux" ] }:


let
  pkgs = if (builtins.getEnv "NIX_PATH_LOCKED" == "1") then
    pkgs'
  else
    import (import ../lib.nix).fetchNixPkgs {};
  iohkpkgs = import ./../default.nix {};
in with pkgs; rec {
  inherit (iohkpkgs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static cardano-sl;
  stack2nix = iohkpkgs.callPackage ./../pkgs/stack2nix.nix {};
  cardano-node-image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./../deployments/cardano-nodes.nix).node0
      (import ./../modules/amazon-image.nix)
      # Just build packages
      ({ lib, ...}: { services.cardano-node.enable = lib.mkForce false;})
    ];
  }).config.system.build.amazonImage;
  tests = import ./../tests { inherit pkgs supportedSystems; };
}
