{ accessKeyId, deployerIP, systemStart, environment, ... }:

with (import ./../lib.nix);
let
  nodeArgs = (import ./cardano-nodes-config.nix { inherit accessKeyId deployerIP systemStart  environment; }).nodeArgs;
  nodeConf = import ./../modules/cardano-node-development.nix;
in
  mkNodesUsing (params: nodeConf) nodeArgs
