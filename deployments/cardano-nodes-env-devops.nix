{ globals, ... }: with import ../lib.nix;
let nodeMap = globals.nodeMap; in


(flip mapAttrs nodeMap (name: import ../modules/cardano-devops.nix))
// {
  network.description = "Cardano Testnet";

  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };
}
