{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/basics.nix
    ../modules/buildkite-agent.nix
  ];
}
