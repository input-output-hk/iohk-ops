{ pkgs, lib, config, ... }:

let
  cfg = config.services.exchange-monitor;
in {
  options = {
    services.exchange-monitor = {
      enable = lib.mkEnableOption "exchange monitor";
    };
  };
  config = lib.mkIf cfg.enable {
    users.users.exchange-monitor = {
      home = "/var/empty";
      isSystemUser = true;
    };
    systemd.services.exchange-monitor = {
      wantedBy = [ "multi-user.target" ];
      script = ''
        exec ${pkgs.callPackage ./exchange-monitor {}}
      '';
      serviceConfig = {
        User = "exchange-monitor";
        Restart = "always";
        RestartSec = "15s";
      };
    };
    services.prometheus2.scrapeConfigs = [
      {
        job_name = "exchange-monitor";
        scrape_interval = "60s";
        metrics_path = "/";
        static_configs = [
          {
            targets = [
              "localhost:8000"
            ];
            labels = { alias = "exchange-monitor"; };
          }
        ];
      }
    ];
  };
}
