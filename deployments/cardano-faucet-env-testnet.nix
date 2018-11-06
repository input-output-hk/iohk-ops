{ globals, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in

{
  faucet = (import ../modules/cardano-testnet.nix) nodeMap.faucet;
  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
    datadogMonitors = with import ../modules/datadog-monitors.nix; {
      cardano_faucet_process = mkMonitor (cardano_faucet_process_monitor // {
        message = pagerDutyPolicy.nonCritical;
        monitorOptions.thresholds = {
          warning = 3;
          critical = 4;
          ok = 1;
        };
      });

      cardano_faucet_balance_monitor = let
        critical = "10000000000"; # amounts in lovelace, average withdrawal is 1000 ada
      in mkMonitor {
        name = "Testnet faucet wallet balance is low";
        type = "metric alert";
        query = config: "max(last_5m):avg:faucet.wallet_balance{depl:${config.deployment.name}} by {host} <= ${critical}";
        monitorOptions.thresholds = {
          warning = "100000000000";
          inherit critical;
        };
      };
    };
  };
}
