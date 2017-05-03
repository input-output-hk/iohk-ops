with (import ./../lib.nix);

let
  nodes = import ./cardano-nodes-config.nix;
  nodeStagConf = import ./../modules/cardano-node-staging.nix;
in {
  resources = {
    elasticIPs = mkNodeIPs nodes accessKeyId;
  };
} // (mkNodes nodes (i: r: nodeStagConf))
