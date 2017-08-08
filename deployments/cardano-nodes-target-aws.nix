{ accessKeyId, deployerIP, ... }:

with (import ./../lib.nix);
let
  nodeInfo    = (import ./cardano-nodes-config.nix         { inherit accessKeyId deployerIP; });
  nodeTgtConf = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId; inherit (nodeInfo) relays; };
in
  mkNodesUsing nodeTgtConf nodeInfo.nodeArgs
