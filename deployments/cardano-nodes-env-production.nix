{ globals, ... }: with (import ./../lib.nix);
let nodeMap = globals.nodeMap; in


(flip mapAttrs nodeMap (name: import ./../modules/cardano-production.nix))
// {
  network.description = "Mainnet";

  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor cpu_monitor;
      disk = mkMonitor disk_monitor;
      ram = mkMonitor ram_monitor;
      ntp = mkMonitor ntp_monitor;
      cardano_node_simple_process = mkMonitor cardano_node_simple_process_monitor;
      chain_quality = mkMonitor chain_quality_monitor;
      failed_cherish_loop = mkMonitor failed_cherish_loop_monitor;
      mem_pool_size = mkMonitor mem_pool_size_monitor;
    });
  };
}
