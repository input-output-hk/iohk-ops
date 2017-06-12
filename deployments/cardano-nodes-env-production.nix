with (import ./../lib.nix);

let
  nodes = import ./cardano-nodes-config.nix;
  nodeProdConf = import ./../modules/cardano-node-prod.nix;
in {
  resources = {
    elasticIPs = mkNodeIPs nodes accessKeyId;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor cpu_monitor;
      disk = mkMonitor disk_monitor;
      ram = mkMonitor ram_monitor;
      ntp = mkMonitor ntp_monitor;

      cardano_node_process_monitor = mkMonitor {
        name = "cardano-node process is down";
        type = "service check";
        query = _config: "\"process.up\".over(\"cardano-node\",\"process:cardano-node\").by(\"host\",\"process\").last(2).count_by_status()";
        monitorOptions.thresholds = {
          warning = 1;
          critical = 1;
        };
      };
    });
  };
} // (mkNodes nodes (i: r: nodeProdConf))
