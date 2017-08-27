{ ... }:

with (import ./../lib.nix);

{ config, ...}:
let nodeMap = config.system.build.iohk.nodeMap;
in {
  resources.elasticIPs = nodesElasticIPs nodeMap;
} // (mkNodesUsing (params: import ./../modules/cardano-node-development.nix) nodeMap)
