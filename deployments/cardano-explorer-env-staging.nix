{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) explorer; }; in


(flip mapAttrs nodeMap (name: import ./../modules/cardano-staging.nix))
//
{
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };
}
