{ pkgs ? (import <nixpkgs> {})}:

with pkgs;

rec {
  image = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      (import ./deployments/cardano.nix).node1
      (import ./modules/amazon-image.nix)
      ({ ec2.hvm = true; })
    ];
  }).config.system.build.amazonImage;

  #node-ec2-image =
  #  runCommand "node-ec2-image" { preferLocalBuild = true; } ''
  #     mkdir -p $out/nix-support
  #     xz -z -c ${image}/nixos.qcow2 > $out/node.img.xz
  #     echo "file img $out/node.img.xz" > $out/nix-support/hydra-build-products
  #  '';
}
