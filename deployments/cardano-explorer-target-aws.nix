{ accessKeyId, deployerIP, systemStart, topologyYaml, environment, ... }:

with (import ./../lib.nix);
let
  nodeTgtConf = import ./../modules/cardano-node-aws.nix { inherit accessKeyId topologyYaml environment; };
  explorer    = import ./cardano-explorer-config.nix { inherit accessKeyId environment deployerIP systemStart; };
in
  mkNodesUsing nodeTgtConf { explorer = explorer; } //
  {
  }
