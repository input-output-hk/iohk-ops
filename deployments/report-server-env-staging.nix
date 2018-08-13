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
      accountName = "iohktrial2";  # NB. expires on 2018-09-08
      email = "staging-report-server@iohk.io";
      tokenFile = "/var/lib/keys/zendesk-token";
    };
  };

  resources.elasticIPs = nodesElasticIPs nodeMap;
  resources.datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
    cardano_report_proccess = mkMonitor (recursiveUpdate cardano_report_process_monitor {
      monitorOptions.thresholds = {
        warning = 3;
        critical = 4;
        ok = 1;
      };
    });
  });
}
