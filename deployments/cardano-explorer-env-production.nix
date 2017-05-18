with (import ./../lib.nix);

let
  nodeProdConf = import ./../modules/cardano-node-prod.nix;
in {
  sl-explorer = nodeProdConf;
  resources = {
    elasticIPs = {
      nodeip40 = { inherit region accessKeyId; };
    };
  };
}
