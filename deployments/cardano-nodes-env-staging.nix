with (import ./../lib.nix);

let
  nodes = import ./cardano-nodes-config.nix;
  nodeStagConf = import ./../modules/cardano-node-staging.nix;
in {
  resources = {
    elasticIPs = mkNodeIPs nodes accessKeyId;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor (cpu_monitor // {
        message = pagerDutyPolicy.nonCritical;
        query = config: "avg(last_5m):avg:system.load.norm.1{env:${config.deployment.name}} by {host} > 0.99";
        monitorOptions.thresholds = {
          warning = "0.99";
          critical = "0.99";
        };
      });

      cardano_node_process = mkMonitor (cardano_node_process_monitor // {
        message = pagerDutyPolicy.nonCritical;
        monitorOptions.thresholds = {
          warning = 4;
          critical = 4;
          ok = 1;
        };
      });
    });
  };
} // (mkNodes nodes (i: r: nodeStagConf))
