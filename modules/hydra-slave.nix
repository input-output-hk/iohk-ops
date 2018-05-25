{ config, pkgs, lib, ... }:

with lib;

let
  iohk-pkgs = import ../default.nix {};
  localLib = import ../lib.nix;
in
{
  imports = [ ./auto-gc.nix ./nix_nsswitch.nix ];
  services = {
    dd-agent.tags = ["group:hydra-and-slaves"];
  };

  nix = {
    binaryCaches = mkForce [ "https://cache.nixos.org" ];
  };

  environment.systemPackages = [ iohk-pkgs.iohk-ops ];

  users.extraUsers.root.openssh.authorizedKeys.keys = map (key: ''
    command="nice -n20 nix-store --serve --write" ${key}
  '') localLib.buildSlaveKeys.linux;
}
