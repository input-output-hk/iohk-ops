{ pkgs, lib, config, ... }:

let
  cfg = config.services.exchange-monitor;
in {
  options = {
    services.exchange-monitor = {
      binance = lib.mkEnableOption "binance exchange monitor";
    };
  };
  config = lib.mkIf cfg.binance {
    users.users.exchange-monitor-binance = {
      home = "/var/empty";
      isSystemUser = true;
    };
    systemd.services.exchange-monitor-binance = {
      wantedBy = [ "multi-user.target" ];
      script = ''
        exec ${pkgs.callPackage ./exchange-monitor {}}
      '';
      serviceConfig = {
        User = "exchange-monitor-binance";
        Restart = "always";
        RestartSec = "15s";
      };
    };
    services.prometheus2.scrapeConfigs = [
      {
        job_name = "exchange-monitor-binance";
        scrape_interval = "60s";
        metrics_path = "/";
        static_configs = [
          {
            targets = [
              "localhost:8000"
            ];
            labels = { alias = "exchange-monitor-binance"; };
          }
        ];
      }
    ];
  };
}
