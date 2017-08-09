{ accessKeyId, deployerIP, topologyYaml, ... }:

with (import ./../lib.nix);
let
  nodeInfo    = (import ./cardano-nodes-config.nix         { inherit accessKeyId deployerIP; });
  nodeTgtConf = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId topologyYaml; inherit (nodeInfo) relays; };
in
  mkNodesUsing nodeTgtConf nodeInfo.nodeArgs
