{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

let
  iohkpkgs = import ./../default.nix;
in rec {
  image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./../deployments/cardano.nix).node0
      (import ./../modules/amazon-image.nix)
      ({ ec2.hvm = true; })
    ];
  }).config.system.build.amazonImage;

  inherit (iohkpkgs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static;
}
