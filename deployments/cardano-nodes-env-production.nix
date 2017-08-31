{ globals, ... }: with (import ./../lib.nix);



(flip mapAttrs globals.nodeMap (name: import ./../modules/cardano-node-production.nix globals))
// {
  resources = {
    elasticIPs = nodesElasticIPs globals.nodeMap;
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
