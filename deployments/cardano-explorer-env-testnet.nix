{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) explorer; }; in


(flip mapAttrs nodeMap (name: import ./../modules/cardano-testnet.nix))
//
{
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cardano_explorer_process = mkMonitor (cardano_explorer_process_monitor // {
        message = pagerDutyPolicy.nonCritical;
        monitorOptions.thresholds = {
          warning = 3;
          critical = 4;
          ok = 1;
        };
      });
    });
  };
}
