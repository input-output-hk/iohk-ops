{ config, lib, pkgs, ... }:

with lib;

{

  imports =
    [ <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
      <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ];

  system.build.amazonImage = import <nixpkgs/nixos/lib/make-disk-image.nix> {
    inherit lib config;
    pkgs = import <nixpkgs> { inherit (pkgs) system; }; # ensure we use the regular qemu-kvm package
    partitioned = config.ec2.hvm;
    diskSize = 6144;
    format = "qcow2";
    configFile = pkgs.writeText "configuration.nix"
      ''
        {
          imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
          ${optionalString config.ec2.hvm ''
            ec2.hvm = true;
          ''}
        }
      '';
  };

}
