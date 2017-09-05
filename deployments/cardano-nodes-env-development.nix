{ globals, ... }: with (import ./../lib.nix);



(flip mapAttrs globals.nodeMap (name: import ./../modules/cardano-development.nix))
// {
  resources.elasticIPs = nodesElasticIPs globals.nodeMap;
}
