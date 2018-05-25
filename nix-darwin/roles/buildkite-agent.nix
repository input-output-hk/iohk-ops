{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/basics.nix
    ../modules/datadog.nix
    ../modules/buildkite-agent.nix
  ];

  services = {
    dd-agent.tags = ["group:buildkite-agents" "group:macos"];
  };
}
