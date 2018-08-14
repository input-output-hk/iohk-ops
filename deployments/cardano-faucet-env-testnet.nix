{ globals, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in


(flip mapAttrs nodeMap (name: import ../modules/cardano-testnet.nix))
//
{
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      cardano_faucet_process = mkMonitor (cardano_faucet_process_monitor // {
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
