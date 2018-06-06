{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in


{
  faucet = { config, resources, ...}: {
    imports = [
      ./../modules/staging.nix
      ./../modules/datadog.nix
    ];

  };
  resources.elasticIPs = nodesElasticIPs nodeMap;
}
