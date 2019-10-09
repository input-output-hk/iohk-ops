let
  niv = import <niv>;
in
{

  defaults = { ... }: {
    deployment.targetEnv = "virtualbox";
    deployment.virtualbox.memorySize = 1024;
  };

  a-node = { pkgs, ... }: {
    imports = [
      (import (niv.cardano-node + "/nix/nixos/cardano-node-instanced.nix") { instances = [ 3000 3001 3002 ]; })
    ];
    services.cardano-node-instanced = {
      enable = true;
      topology = ./instanced-topology.json;
    };
  };
}
