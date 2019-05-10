{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) report-server; }; in

{
  report-server = { config, resources, lib, ...}: {
    imports = [
      ../modules/testnet.nix
    ];
  };

  resources.elasticIPs = nodesElasticIPs nodeMap;
}
