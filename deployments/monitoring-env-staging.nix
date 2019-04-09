{
  require = [ ./monitoring.nix ];
  monitoring = { ... }: 
  {
    imports = [
      ../modules/staging.nix
    ];
    services.monitoring-services.applicationDashboards = ../modules/grafana/cardano;
    services.monitoring-services.applicationRules = [
      {
        alert = "chain_quality_degraded";
        expr = "cardano_chain_quality_last_k__2160__blocks__ < 99";
        for = "5m";
        labels = {
          severity = "page";
        };
        annotations = {
          summary = "{{$labels.alias}}: Degraded Chain Quality over last 2160 blocks.";
          description = "{{$labels.alias}}: Degraded Chain Quality over last 2160 blocks (<99%).";
        };
      }
      {
        alert = "mempoolsize_tx_count_too_large";
        expr = "max_over_time(cardano_MemPoolSize[5m]) > 190";
        for = "1m";
        labels = {
          severity = "page";
        };
        annotations = {
          summary = "{{$labels.alias}}: MemPoolSize tx count is larger than expected.";
          description = "{{$labels.alias}}: When a node's MemPoolSize grows larger than the system can handle, transactions will be dropped. The actual thresholds for that in mainnet are unknown, but [based on benchmarks done beforehand](https://input-output-rnd.slack.com/archives/C2VJ41WDP/p1506563332000201) transactions started getting dropped when the MemPoolSize was ~200 txs.";
        };
      }
    ];
  };
}