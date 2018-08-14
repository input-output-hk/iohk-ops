# This is the basic setup of a deployer host.
# The different deployers will add their own users and config on top.
#
# The thing about a deployer host is that it deploys itself through
# the nixops None backend, and just uses plain nixops, not the
# iohk-ops wrapper.

with (import ../lib.nix);

let
  iohk-pkgs = import ../default.nix {};

in

{ config, pkgs, ... }: {

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = (with iohk-pkgs; [
    iohk-ops
    terraform
    mfa
  ]) ++ (with pkgs; [
    psmisc
    gnupg
    nixops
    awscli
    jq
    yq
    python3
    htop
  ]);

  users.groups.deployers = {};

  users.users = {
    # Re-deploy the deployer host itself,
    # and apply global terraform.
    deployer = {
      isNormalUser = true;
      description  = "Deploy the deployer";
      group        = "deployers";
      extraGroups  = [ "wheel" ];
      openssh.authorizedKeys.keys = devOpsKeys;
    };
  };
}
