{ accessKeyId, deployerIP, ... }:

with (import ./../lib.nix);
let
  nodeArgs = (import ./cardano-nodes-config.nix { inherit accessKeyId deployerIP; }).nodeArgs;
  nodeConf = import ./../modules/cardano-node-development.nix;
in {
  resources = rec {
    elasticIPs = mkNodeIPs nodeArgs accessKeyId;
  };
} // (mkNodesUsing (params: nodeConf) nodeArgs)
