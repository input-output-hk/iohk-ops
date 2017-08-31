with (import ./../lib.nix);

globals: params:
{ config, pkgs, lib, ... }:

let
  report-server-drv = (import ./../default.nix {}).cardano-report-server-static;
in {
  imports = [ ./common.nix ];

  options.services.report-server = {
      logsdir = mkOption { type = types.path; default = "/var/lib/report-server"; };
  };

  config = {
    networking.firewall.allowedTCPPorts = [
      params.port
    ];

    users = {
      users.report-server = {
        group = "report-server";
        home = config.services.report-server.cfg.logsdir;
        createHome = true;
      };
      groups.report-server = {};
    };

    systemd.services.report-server = {
      description   = "Cardano report server";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "report-server";
        Group = "report-server";
        ExecStart = ''
          ${report-server-drv}/bin/cardano-report-server -p ${toString params.port} --logsdir ${config.services.report-server.logsdir}
        '';
      };
    };
  };
}
