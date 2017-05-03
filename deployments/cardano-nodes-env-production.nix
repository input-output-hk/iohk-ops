with (import ./../lib.nix);

let
  nodes = import ./cardano-nodes-config.nix;
  nodeProdConf = import ./../modules/cardano-node-prod.nix;
in {
  resources = {
    elasticIPs = mkNodeIPs nodes accessKeyId;
  };
} // (mkNodes nodes (i: r: nodeProdConf))
