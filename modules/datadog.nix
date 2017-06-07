{ config, pkgs, lib, ... }:

with (import ./../lib.nix);

let
  appKey = "???";
  apiKey = fileContents ./../static/datadog.secret;
  baseMonitorOptions = builtins.toJSON {
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
  config = {
    services.dd-agent = {
      enable = true;
      api_key = apiKey;
      hostname = config.networking.hostName;
    };
  };

  resources.datadogMonitors = {
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
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.2";
        };
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
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.9";
        };
      };

    non_crit_cpu_monitor = { config, ... }:
      {
        inherit apiKey appKey;

        name = "Non-Critical: high CPU usage";
        type = "metric alert";
        message = messages.non_critical;
        query = "avg(last_5m):avg:system.load.norm.1{host:${config.deployment.name}.machine} by {host} > 0.75";
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.75";
        };
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
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.9";
        };
      };

    non_crit_disk_monitor = { config, ... }:
      {
        inherit apiKey appKey;

        name = "Non-Critical: high disk usage";
        type = "metric alert";
        message = messages.non_critical;
        query = "max(last_5m):avg:system.disk.in_use{host:${config.deployment.name}.machine} by {host} > 0.8";
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.8";
        };
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
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.2";
        };
      };

    non_crit_ram_monitor = { config, ... }:
      {
        inherit apiKey appKey;

        name = "Non-Critical: RAM is running low";
        type = "metric alert";
        message = messages.non_critical;
        query = "avg(last_1m):avg:system.mem.pct_usable{host:${config.deployment.name}.machine} by {host} < 0.5";
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = "0.5";
        };
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
        monitorOptions = baseMonitorOptions // builtins.toJSON {
          thresholds.critical = 1;
        };
      };
 };
}
