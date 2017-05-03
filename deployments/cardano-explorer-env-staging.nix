with (import ./../lib.nix);

let
  nodeStagingConf = import ./../modules/cardano-node-staging.nix;
in {
  sl-explorer = nodeStagingConf;
  resources = {
    elasticIPs = {
      nodeip40 = { inherit region accessKeyId; };
    };
  };
}
