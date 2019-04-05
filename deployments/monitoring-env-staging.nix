{
  require = [ ./monitoring.nix ];
  monitoring = { ... }: 
  {
    imports = [
      ../modules/staging.nix
    ];
    services.monitoring-services.applicationDashboards = [
      ../modules/grafana/cardano-application-dashboard.json
      ../modules/grafana/cardano-failures.dashboard.json
    ];
  };
}