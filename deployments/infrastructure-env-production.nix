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
	pagerdutyMessage = ''
	  {{#is_alert}}
	  @pagerduty-Datadog_Critical
	  {{/is_alert}}

	  {{#is_warning}}
	  @pagerduty-Datadog_Non-Critical
	  {{/is_warning}}
	'';
      in
      {
        # ---------------------------------------------------------------------------
        # cardano-node process
        #

        cardano_node_process_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "cardano-node process is down";
            type = "service check";
            message = pagerdutyMessage;
            query = "\"process.up\".over(\"cardano-node\",\"process:cardano-node\").by(\"host\",\"process\").last(2).count_by_status()";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
	      thresholds.warning = 1;
              thresholds.critical = 1;
            });
          };

        # ---------------------------------------------------------------------------
        # CPU
        #

        cpu_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "High CPU usage";
            type = "metric alert";
            message = pagerdutyMessage;
            query = "avg(last_5m):avg:system.load.norm.1{host:${config.deployment.name}.machine} by {host} > 0.9";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
	      thresholds.warning = "0.75";
              thresholds.critical = "0.9";
            });
          };

        # ---------------------------------------------------------------------------
        # Disk
        #

        disk_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "High disk usage";
            type = "metric alert";
            message = pagerdutyMessage;
            query = "max(last_5m):avg:system.disk.in_use{host:${config.deployment.name}.machine} by {host} > 0.9";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.warning = "0.8";
              thresholds.critical = "0.9";
            });
          };

        # ---------------------------------------------------------------------------
        # RAM
        #

        ram_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "RAM is running low";
            type = "metric alert";
            message = pagerdutyMessage;
            query = "avg(last_1m):avg:system.mem.pct_usable{host:${config.deployment.name}.machine} by {host} < 0.2";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.warning = "0.5";
              thresholds.critical = "0.2";
            });
          };

        # ---------------------------------------------------------------------------
        # NTP
        #

        ntp_monitor = { config, ... }:
          {
            inherit apiKey appKey;

            name = "Clock out of sync with NTP";
            type = "service check";
            message = pagerdutyMessage;
            query = "\"ntp.in_sync\".over(\"{host:${config.deployment.name}.machine}\").by(\"host\").last(2).count_by_status()";
            monitorOptions = builtins.toJSON (baseMonitorOptions // {
              thresholds.critical = 1;
            });
          };
      };
  };
}
