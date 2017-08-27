{ ... }:

with (import ./../lib.nix);

{ config, ... }:

(mkNodesUsing (params: import ./../modules/cardano-node-prod.nix) config.system.build.iohk.nodeMap)
// {
  resources = {
    elasticIPs = nodesElasticIPs config.system.build.iohk.nodeMap;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor cpu_monitor;
      disk = mkMonitor disk_monitor;
      ram = mkMonitor ram_monitor;
      ntp = mkMonitor ntp_monitor;
      cardano_node_simple_process = mkMonitor cardano_node_simple_process_monitor;
      cardano_explorer_process = mkMonitor cardano_explorer_process_monitor;
    });
  };
}
