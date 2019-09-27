{
  require = [ ./monitoring.nix ];
  monitoring = { pkgs, lib, ... }:
  {
    imports = [
      ../modules/staging.nix
    ];

    systemd.services.graylog.environment = { JAVA_OPTS = ''
      -Djava.library.path=${pkgs.graylog}/lib/sigar -Xms3g -Xmx3g -XX:NewRatio=1 -server -XX:+ResizeTLAB -XX:+UseConcMarkSweepGC -XX:+CMSConcurrentMTEnabled -XX:+CMSClassUnloadingEnabled -XX:+UseParNewGC -XX:-OmitStackTraceInFastThrow
    ''; };
    services.elasticsearch.extraJavaOptions = [ "-Xms6g" "-Xmx6g" ];
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
      {
        alert = "jormungandr_block_divergence";
        expr = "max(jormungandr_lastBlockHeight) - ignoring(alias,instance,job,role) group_right(instance) jormungandr_lastBlockHeight > 20";
        for = "5m";
        labels = {
          severity = "page";
        };
        annotations = {
          summary = "{{$labels.alias}}: Jormungandr block divergence detected for more than 5 minutes";
          description = "{{$labels.alias}}: Jormungandr block divergence detected for more than 5 minutes";
        };
      }
      {
        alert = "jormungandr_blockheight_unchanged";
        expr = "rate(jormungandr_lastBlockHeight[5m]) == 0";
        for = "10m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}} Jormungandr blockheight unchanged for >=10mins";
          description = "{{$labels.alias}} Jormungandr blockheight unchanged for >=10mins.";
        };
      }
      {
        alert = "jormungandr_faucetFunds_monitor";
        expr = "jormungandr_faucetFunds < 10000000000000";
        for = "5m";
        labels.severity = "page";
        annotations = {
          description = "{{$labels.alias}} Jormungandr faucet wallet balance is low (< 10M ADA)";
        };
      }
    ];
  };
}
