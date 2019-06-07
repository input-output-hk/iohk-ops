{ globals, ... }: with import ../lib.nix;
let nodeMap = globals.nodeMap; in


(flip mapAttrs nodeMap (name: import ../modules/cardano-production.nix))
// {
  network.description = "Mainnet";

  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = with import ../modules/datadog-monitors.nix; {
      #inodes = mkMonitor (inodes_monitor 7 2);
      #mem_pool_size = mkMonitor mem_pool_size_monitor;
    };
  };
}
