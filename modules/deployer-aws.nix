# This is the basic setup of a deployer host.
# The different deployers will add their own users and config on top.
#
# The thing about a deployer host is that it deploys itself through
# the nixops None backend, and just uses plain nixops, not the
# iohk-ops wrapper.

{ lib, ... }: {
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  deployment = {
    targetEnv = "none";
    targetHost = "localhost";
  };

  # Don't reconfigure the system from EC2 userdata on next startup
  systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
}
