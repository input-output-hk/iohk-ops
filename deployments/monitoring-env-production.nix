{
  require = [ ./monitoring.nix ];
  defaults = {
    services.monitoring-exporters = {
      # TODO monitoring-ip should come from monitoring-target-aws.nix
      graylogHost = "monitoring-ip:5044";
    };
  };
  monitoring = { pkgs, lib, ... }:
  {
    imports = [
      ../modules/production.nix
    ];

    deployment.ec2.instanceType = "t3.xlarge";
    boot.loader.grub.device = lib.mkForce "/dev/nvme0n1"; # t3.xlarge has an nvme disk, and amazon-image.nix isnt handling it right yet
    deployment.ec2.ebsInitialRootDiskSize = 1000;

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
    ];
  };
}
