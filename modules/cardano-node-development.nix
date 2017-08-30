with (import ./../lib.nix);

{ config, pkgs, resources, ... }: {
  services.cardano-node = {
    saveCoreDumps = true;
  };
}
