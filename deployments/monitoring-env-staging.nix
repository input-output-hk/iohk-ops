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
          summary = "{{$labels.alias}}: Chain quality last 2160 blocks degraded.";
          description = "{{$labels.alias}}: Chain quality last 2160 blocks degraded (<99%).";
        };
      }
    ];
  };
}