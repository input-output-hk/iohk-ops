{ nodeLimit, ... }:

with (import ./../lib.nix);
let
  nodeConfig = import ./../modules/cardano-node-config.nix;
  nodes = import ./cardano-nodes-config.nix { inherit nodeLimit; };
in {
  network.description = "Cardano SL";
} // (mkNodes nodes nodeConfig)
