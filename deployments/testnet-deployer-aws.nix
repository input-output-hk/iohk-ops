{ ... }:

with (import ./../lib.nix);

{
  testnet-deployer = { config, pkgs, lib, resources, ... }: {
    imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
    ec2.hvm = true;

    deployment = {
      targetEnv = "none";
      targetHost = "localhost";
    };

    # Don't reconfigure the system from EC2 userdata on next startup
    systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
  };
}
