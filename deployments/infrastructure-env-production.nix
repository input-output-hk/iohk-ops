with (import ./../lib.nix);

{
  network.description = "IOHK infrastructure production";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/papertrail.nix
      ./../modules/datadog.nix
    ];

    services.dd-agent.tags = ["env:production"];

    deployment.ec2 = {
      elasticIPv4 = resources.elasticIPs.hydra-ip;
    };
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/common.nix
      ./../modules/amazon-base.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:production"];

    deployment.ec2 = {
      elasticIPv4 = resources.elasticIPs.cardanod-ip;
    };
  };

  resources = {
    elasticIPs = {
      hydra-ip = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
    };
    datadogMonitors =
      let
        appKey = fileContents ./../static/datadog-application.secret;
        apiKey = fileContents ./../static/datadog-api.secret;
        baseMonitorOptions = {
          renotify_interval = 60;
          include_tags = true;
          no_data_timeframe = 10;
          notify_audit = true;
          notify_no_data = false;
          require_full_window = true;
        };
        messages = {
          critical = "notify @pagerduty-Datadog_Critical";
          non_critical = "notify @pagerduty-Datadog_Non-Critical";
        };
      in
      {
        # ---------------------------------------------------------------------------
        # cardano-node process
        #

        crit_cardano_node_process_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Critical: cardano-node process is down";
            type = "metric alert";
            message = messages.critical;
            query = "\"process.up\".over(\"cardano-node\",\"process:cardano-node\").by(\"host\",\"process\").last(2).count_by_status()";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.2";
            });
          };

        # ---------------------------------------------------------------------------
        # CPU
        #

        crit_cpu_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Critical: high CPU usage";
            type = "metric alert";
            message = messages.critical;
            query = "avg(last_5m):avg:system.load.norm.1{host:${config.deployment.name}.machine} by {host} > 0.9";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.9";
            });
          };

        non_crit_cpu_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Non-Critical: high CPU usage";
            type = "metric alert";
            message = messages.non_critical;
            query = "avg(last_5m):avg:system.load.norm.1{host:${config.deployment.name}.machine} by {host} > 0.75";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.75";
            });
          };

        # ---------------------------------------------------------------------------
        # Disk
        #

        crit_disk_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Critical: high disk usage";
            type = "metric alert";
            message = messages.critical;
            query = "max(last_5m):avg:system.disk.in_use{host:${config.deployment.name}.machine} by {host} > 0.9";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.9";
            });
          };

        non_crit_disk_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Non-Critical: high disk usage";
            type = "metric alert";
            message = messages.non_critical;
            query = "max(last_5m):avg:system.disk.in_use{host:${config.deployment.name}.machine} by {host} > 0.8";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.8";
            });
          };

        # ---------------------------------------------------------------------------
        # RAM
        #

        crit_ram_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Critical: RAM is running low";
            type = "metric alert";
            message = messages.critical;
            query = "avg(last_1m):avg:system.mem.pct_usable{host:${config.deployment.name}.machine} by {host} < 0.2";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.2";
            });
          };

        non_crit_ram_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Non-Critical: RAM is running low";
            type = "metric alert";
            message = messages.non_critical;
            query = "avg(last_1m):avg:system.mem.pct_usable{host:${config.deployment.name}.machine} by {host} < 0.5";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = "0.5";
            });
          };

        # ---------------------------------------------------------------------------
        # NTP
        #

        crit_ntp_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Critical: Clock out of sync with NTP";
            type = "service check";
            message = messages.critical;
            query = "\"ntp.in_sync\".over(\"{host:${config.deployment.name}.machine}\").by(\"host\").last(2).count_by_status()";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = 1;
            });
          };
      };
  };
}
