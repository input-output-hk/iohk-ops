{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/basics.nix
    ../modules/datadog.nix
    ../modules/hydra-slave.nix
  ];

  services = {
   dd-agent.tags = ["group:hydra-and-slaves" "group:macos"];
  };
}
