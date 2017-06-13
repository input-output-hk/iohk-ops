with (import ./../lib.nix);

let
  nodes = import ./cardano-nodes-config.nix;
  nodeStagConf = import ./../modules/cardano-node-staging.nix;
in {
  resources = {
    elasticIPs = mkNodeIPs nodes accessKeyId;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor (cpu_monitor // {
        monitorOptions.thresholds = {
	  warning = "0.95";
	  critical = "0.99";
        };
      });

      cardano_node_process_monitor = mkMonitor {
        name = "cardano-node process is down";
        type = "service check";
        query = _config: "\"process.up\".over(\"cardano-node\",\"process:cardano-node\").by(\"host\",\"process\").last(2).count_by_status()";
        monitorOptions.thresholds = {
          warning = 2;
          critical = 4;
        };
      };
    });
  };
} // (mkNodes nodes (i: r: nodeStagConf))
