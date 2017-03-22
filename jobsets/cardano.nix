{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

rec {
  # TODO:
  # in job ‘image’:
  # The option `deployment' defined in `/nix/store/pp4mi8hmrk923kf18x0j88v35v0lx59z-git-export/modules/common.nix' does not exist.

  #image = (import <nixpkgs/nixos/lib/eval-config.nix> {
  #  system = "x86_64-linux";
  #  modules = [
  #    (import ./../deployments/cardano.nix).node0
  #    (import ./../modules/amazon-image.nix)
  #    ({ ec2.hvm = true; })
  #  ];
  #}).config.system.build.amazonImage;

  cardano-sl = import ./../default.nix;
}
