{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

rec {
  image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./../deployments/cardano.nix).node0
      (import ./../modules/amazon-image.nix)
      ({ ec2.hvm = true; })
    ];
  }).config.system.build.amazonImage;

  cardano-sl = import ./../default.nix;
}
