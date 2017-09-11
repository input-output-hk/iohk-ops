{ globals, ... }: with (import ./../lib.nix);



(flip mapAttrs globals.nodeMap (name: import ./../modules/cardano-development.nix))
// {
  network.description = "Cardano Development";

  resources.elasticIPs = nodesElasticIPs globals.nodeMap;
}
