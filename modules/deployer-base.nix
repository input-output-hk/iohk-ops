with (import ../lib.nix);

{ config, pkgs, ... }: {
  imports = [ ./common.nix ];

  networking.hostName = "deployer-${config.deployment.name}";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = (with pkgs; [
    awscli
    gnupg
    htop
    jq
    nixops
    psmisc
    python3
    vim
    yq
  ]);

  users.groups.deployers = {};
  users.users.deployer = (import ../lib/users/deployer-users.nix).deployer;
}
