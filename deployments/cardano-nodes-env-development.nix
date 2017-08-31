{ globals, ... }: with (import ./../lib.nix);



(flip mapAttrs globals.nodeMap (name: import ./../modules/cardano-node-development.nix globals))
// {
  resources = {
    elasticIPs = nodesElasticIPs globals.nodeMap;
  };
}
