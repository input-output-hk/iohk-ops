{ accessKeyId, deployerIP, systemStart, ... }:

let
  explorer = (import ./cardano-nodes-config.nix { inherit accessKeyId deployerIP systemStart; }).explorer;
in
with (import ./../lib.nix);
{
  network.description = "Cardano Explorer";

  explorer = {
    imports = [
      # A node with 1) index 40, 2) no region and 3) in outer tier:
      (import ./../modules/cardano-node-config.nix explorer)
      ./../modules/cardano-explorer.nix
    ];
  };
}
