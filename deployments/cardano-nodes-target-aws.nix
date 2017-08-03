{ accessKeyId, deployerIP, ... }:

with (import ./../lib.nix);
let
  nodeArgs    = (import ./cardano-nodes-config.nix         { inherit accessKeyId deployerIP; }).nodeArgs;
  nodeTgtConf = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId; };
in
  mkNodesUsing nodeTgtConf nodeArgs
