{ globals, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) explorer; }; in

{
  explorer = (import ../modules/cardano-devops.nix) nodeMap.explorer;
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };
}
