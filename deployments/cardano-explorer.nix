{ accessKeyId, deployerIP, environment, systemStart, topologyYaml, ... }:

let
  explorer = import ./cardano-explorer-config.nix { inherit accessKeyId deployerIP environment systemStart; };
in
with (import ./../lib.nix);
{
  network.description = "Cardano Explorer";

  explorer = {
    imports = [
      (import ./../modules/cardano-node-config.nix explorer)
      (import ./../modules/cardano-explorer.nix { inherit environment; })
    ];
  };
}
