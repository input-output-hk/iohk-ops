{ config, pkgs, lib, ... }:

with lib;

let
  iohk-pkgs = import ../default.nix {};
in
{
  imports = [ ./auto-gc.nix ./nix_nsswitch.nix ];
  services = {
    dd-agent.tags              = [" group:hydra-and-slaves"];
  };

  nix = {
    binaryCaches = mkForce [ "https://cache.nixos.org" ];
  };

  environment.systemPackages = [ iohk-pkgs.iohk-ops ];

  users.extraUsers.root.openssh.authorizedKeys.keys = pkgs.lib.singleton ''
    command="nice -n20 ${pkgs.utillinux}/bin/flock -s /var/lock/lab nix-store --serve --write" ${pkgs.lib.readFile ./../static/id_buildfarm.pub}
  '';
}
