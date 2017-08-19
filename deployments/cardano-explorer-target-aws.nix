{ accessKeyId, deployerIP, systemStart, topologyYaml, ... }:

with (import ./../lib.nix);
let
  explorer    = (import ./cardano-nodes-config.nix         { inherit accessKeyId deployerIP systemStart; }).explorer;
  nodeTgtConf = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId topologyYaml; relays = explorer.relays; };
in
  mkNodesUsing nodeTgtConf { explorer = explorer; }
