{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/basics.nix
    ../modules/buildkite-agent.nix
  ];
  services.buildkite-services-darwin.metadata = "system=x86_64-darwin,queue=daedalus";
}
