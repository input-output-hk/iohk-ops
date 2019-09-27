{ pkgs, lib, config, ... }:

let
  cfg = config.services.jormungandr-monitor;
in with lib; {
  options = {
    services.jormungandr-monitor = {
      enable = mkEnableOption "jormungandr monitor";
      faucetAddress = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          An optional faucet address to monitor.
        '';
      };
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
      environment = mkIf (cfg.faucetAddress != null) {
        FAUCET_ADDRESS = cfg.faucetAddress;
      };
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
