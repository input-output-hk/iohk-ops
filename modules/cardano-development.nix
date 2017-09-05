with (import ./../lib.nix);

params:
{ config, pkgs, resources, ... }: {
  imports = [
    ./development.nix
  ];
}
