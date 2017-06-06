{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

let
  iohkpkgs = import ./../default.nix {};
in rec {
  inherit (iohkpkgs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static cardano-sl;
  cardano-node-image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./../deployments/cardano-nodes.nix).node0
      (import ./../modules/amazon-image.nix)
      # Rare exception, for generating AMI we can allow not having initial peers
      ({ services.cardano-node.initialPeers = [];})
    ];
  }).config.system.build.amazonImage;
  stack2nix = iohkpkgs.callPackage ./../pkgs/stack2nix.nix {};
}
