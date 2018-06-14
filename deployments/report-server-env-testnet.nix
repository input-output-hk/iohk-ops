{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) report-server; }; in

{
  report-server = { config, resources, ...}: {
    imports = [
      ./../modules/testnet.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    config.services.report-server.zendesk = {
      accountName = "iohk";
      email = "daedalus-testnet-bug-reports@iohk.io";
      tokenFile = "/var/lib/keys/zendesk-token";
    };
  };

  resources.elasticIPs = nodesElasticIPs nodeMap;
}
