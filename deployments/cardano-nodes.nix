{ accessKeyId, deployerIP, systemStart, environment, ... }:

with (import ./../lib.nix);
let
  nodeArgs   = (import ./cardano-nodes-config.nix { inherit accessKeyId deployerIP systemStart environment; }).nodeArgs;
  nodeConfig = import ./../modules/cardano-node-config.nix;
in {
  network.description = "Cardano SL";
} // (mkNodesUsing nodeConfig nodeArgs)
