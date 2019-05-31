{ config, lib, pkgs, ... }:

{
  # dd-agent module is not in nix-darwin upstream
  imports = [ ../services/caffeinate.nix ];

  services.caffeinate = {
    enable = true;
  };
}
