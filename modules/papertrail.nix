{ config, pkgs, ... }:

{
  systemd.services.papertrail = {
    description = "Papertrail.com log aggregation";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "5s";
      TimeoutStartSec = 0;
      KillSignal = "SIGINT";
      ExecStart = "${pkgs.bash}/bin/bash -c \"${pkgs.systemd}/bin/journalctl -f | ${pkgs.nmap}/bin/ncat --ssl logs5.papertrailapp.com 43689\"";
    };
  };
}
