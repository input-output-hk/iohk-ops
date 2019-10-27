{ config, lib, pkgs, ... }:

with lib;

{
  services.nix-daemon.enable = true;
  services.hercules-ci-agent.enable = true;
}
