with (import ./../lib.nix);

rec {
  mkMonitor = { name, type, query, message ? "", monitorOptions ? {} }:
    { config, ... }:
    {
      inherit name type;

      apiKey = fileContents ./../static/datadog-api.secret;
      appKey = fileContents ./../static/datadog-application.secret;
      query = query config;
      message = ''
        ${message}

        {{#is_alert}}
        @pagerduty-Datadog_Critical
        {{/is_alert}}

        {{#is_warning}}
        @pagerduty-Datadog_Non-Critical
        {{/is_warning}}
      '';
      monitorOptions = builtins.toJSON ({
        renotify_interval = 60;
        include_tags = true;
        no_data_timeframe = 10;
        notify_audit = true;
        notify_no_data = false;
        require_full_window = true;
      } // monitorOptions);
    };

    # Common monitors
    cpu_monitor = mkMonitor {
      name = "High CPU usage";
      type = "metric alert";
      query = config: "avg(last_5m):avg:system.load.norm.1{host:${config.deployment.name}.machine} by {host} > 0.9";
      monitorOptions.thresholds = {
        warning = "0.75";
        critical = "0.9";
      };
    };
    disk_monitor = mkMonitor {
      name = "High disk usage";
      type = "metric alert";
      query = config: "max(last_5m):avg:system.disk.in_use{host:${config.deployment.name}.machine} by {host} > 0.9";
      monitorOptions.thresholds = {
        warning = "0.8";
        critical = "0.9";
      };
    };
    ram_monitor = mkMonitor {
      name = "RAM is running low";
      type = "metric alert";
      query = config: "avg(last_1m):avg:system.mem.pct_usable{host:${config.deployment.name}.machine} by {host} < 0.2";
      monitorOptions.thresholds = {
        warning = "0.5";
        critical = "0.2";
      };
    };
    ntp_monitor = mkMonitor {
      name = "Clock out of sync with NTP";
      type = "service check";
      query = config: "\"ntp.in_sync\".over(\"{host:${config.deployment.name}.machine}\").by(\"host\").last(2).count_by_status()";
      monitorOptions.thresholds = {
        critical = 1;
      };
    };
}
