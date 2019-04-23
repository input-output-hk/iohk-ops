{ globals, ... }: with (import ./../lib.nix);
let nodeMap = globals.nodeMap; in


(flip mapAttrs nodeMap (name: import ./../modules/cardano-staging.nix))
//
{
  network.description = "Cardano Staging";

  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };
}
