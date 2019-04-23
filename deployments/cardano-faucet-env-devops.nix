{ globals, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in

{
  faucet = (import ../modules/cardano-devops.nix) nodeMap.faucet;
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };
}
