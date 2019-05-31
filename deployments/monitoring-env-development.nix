{
  require = [ ./monitoring.nix ];
  monitoring = { ... }:
  {
    imports = [
      ../modules/development.nix
    ];
    services.monitoring-services.applicationDashboards = ../modules/grafana/cardano;
    services.monitoring-services.applicationRules = [ ];
    services.monitoring-services.logging = false;
    services.monitoring-services.metrics = true;
  };
}
