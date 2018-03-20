{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) report-server; }; in

{
  require = [ ./report-server-bucket-storage.nix ];
  report-server = { config, resources, ...}: {
    imports = [
      ./../modules/staging.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    config.services.report-server.zendesk = {
      accountName = "iohksupport";  # NB. expires on 2018-02-26
      email = "staging-report-server@iohk.io";
    };
  };

  resources.elasticIPs = nodesElasticIPs nodeMap;
}
