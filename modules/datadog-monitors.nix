with (import ./../lib.nix);

rec {
  alertMessage = msg: ''
    {{#is_alert}}
    ${msg}
    {{/is_alert}}
  '';
  warningMessage = msg: ''
    {{#is_warning}}
    ${msg}
    {{/is_warning}}
  '';
  pagerDutyCritical = "@pagerduty-Datadog_Critical";
  pagerDutyNonCritical = "@pagerduty-Datadog_Non-Critical";
  pagerDutyPolicy = {
    normal = ''
      ${alertMessage pagerDutyCritical}
      ${warningMessage pagerDutyNonCritical}
    '';
    nonCritical = ''
      ${alertMessage pagerDutyNonCritical}
      ${warningMessage pagerDutyNonCritical}
    '';
  };

  baseMonitor = {
    apiKey = fileContents ./../static/datadog-api.secret;
    appKey = fileContents ./../static/datadog-application.secret;
    monitorOptions = {
      renotify_interval = 60;
      include_tags = true;
      no_data_timeframe = 10;
      notify_audit = true;
      notify_no_data = false;
      require_full_window = true;
    };
  };

  mkMonitor = { name, type, query, message ? pagerDutyPolicy.normal, monitorOptions ? {} }:
    { config, ... }:
    baseMonitor // {
      inherit name type message;
      query = query config;
      monitorOptions = builtins.toJSON (baseMonitor.monitorOptions // monitorOptions);
    };

  # Common monitors
  cpu_monitor = {
    name = "High CPU usage";
    type = "metric alert";
    query = config: "avg(last_5m):avg:system.load.norm.1{env:${config.deployment.arguments.environment}} by {host} > 0.9";
    monitorOptions.thresholds = {
      warning = "0.75";
      critical = "0.9";
    };
  };

  disk_monitor = {
    name = "High disk usage";
    type = "metric alert";
    query = config: "max(last_5m):avg:system.disk.in_use{env:${config.deployment.arguments.environment}} by {host,device} > 0.9";
    monitorOptions.thresholds = {
      warning = "0.8";
      critical = "0.9";
    };
  };

  ram_monitor = {
    name = "RAM is running low";
    type = "metric alert";
    query = config: "avg(last_1m):avg:system.mem.pct_usable{env:${config.deployment.arguments.environment}} by {host} < 0.2";
    monitorOptions.thresholds = {
      warning = "0.5";
      critical = "0.2";
    };
  };

  ntp_monitor = {
    name = "Clock out of sync with NTP";
    type = "service check";
    query = config: "\"ntp.in_sync\".over(\"env:${config.deployment.arguments.environment}\").by(\"host\").last(2).count_by_status()";
    monitorOptions.thresholds = {
      critical = 1;
    };
  };

  cardano_node_simple_process_monitor = {
    name = "cardano-node-simple process is down";
    type = "service check";
    query = config: "\"process.up\".over(\"env:${config.deployment.arguments.environment}\",\"process:cardano-node-simple\").by(\"host\",\"process\").last(5).count_by_status()";
    monitorOptions.thresholds = {
      warning = 2;
      critical = 4;
      ok = 2;
    };
  };

  cardano_explorer_process_monitor = {
    name = "cardano-explorer process is down";
    type = "service check";
    query = config: "\"process.up\".over(\"env:${config.deployment.arguments.environment}\",\"process:cardano-explorer\").by(\"host\",\"process\").last(5).count_by_status()";
    monitorOptions.thresholds = {
      warning = 2;
      critical = 4;
      ok = 2;
    };
  };
}
