with (import ./../lib.nix);

globals: params:
{ config, pkgs, resources, ... }: {
  services.cardano-node = {
    saveCoreDumps = true;
  };
}
