{ accessKeyId, deployerIP, topologyYaml, systemStart, environment, ... }:

with (import ./../lib.nix);
let
  nodeInfo    = (import ./cardano-nodes-config.nix         { inherit accessKeyId deployerIP systemStart environment; });
  nodeTgtConf = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId topologyYaml environment; };
in
  mkNodesUsing nodeTgtConf nodeInfo.nodeArgs
