with import ../lib.nix;

{ config, pkgs, resources, ... }: {
  imports = [
    ./development.nix
  ];
}
