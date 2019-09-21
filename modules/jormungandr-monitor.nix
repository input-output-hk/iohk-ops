{ pkgs, lib, config, ... }:

let
  cfg = config.services.jormungandr-monitor;
in {
  options = {
    services.jormungandr-monitor = {
      enable = lib.mkEnableOption "jormungandr monitor";
    };
  };
  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 8000 ];
    users.users.jormungandr-monitor = {
      home = "/var/empty";
      isSystemUser = true;
    };
    systemd.services.jormungandr-monitor = {
      wantedBy = [ "multi-user.target" ];
      script = ''
        exec ${pkgs.callPackage ./jormungandr-monitor {}}
      '';
      serviceConfig = {
        User = "jormungandr-monitor";
        Restart = "always";
        RestartSec = "15s";
      };
    };
  };
}
