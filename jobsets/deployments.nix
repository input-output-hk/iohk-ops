{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

rec {
  image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./../deployments/cardano-nodes.nix).node0
      (import ./../modules/amazon-image.nix)
      # Rare exception, for generating AMI we can allow not having initial peers
      ({ services.cardano-node.initialPeers = [];})
    ];
  }).config.system.build.amazonImage;
}
