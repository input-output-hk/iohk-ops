{ globals, ... }: with (import ./../lib.nix);
let nodeMap = globals.nodeMap; in


(flip mapAttrs nodeMap (name: import ./../modules/cardano-production.nix))
// {
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cpu = mkMonitor cpu_monitor;
      disk = mkMonitor disk_monitor;
      ram = mkMonitor ram_monitor;
      ntp = mkMonitor ntp_monitor;
      cardano_node_simple_process = mkMonitor cardano_node_simple_process_monitor;
    });
  };
}
