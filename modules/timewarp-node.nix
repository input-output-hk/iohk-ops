{ config, pkgs, lib, nodes, ...}:

with lib;

let
  time-warp = (import ./../default.nix { inherit pkgs; }).tw-rework-sketch;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
  cfg = config.services.timewarp-node;
in {
  options = {
    services.timewarp-node = {
      enable = mkEnableOption "timewarp-node";
      sender = mkOption { type = types.bool; default = false; };
      port = mkOption { type = types.int; default = 3055; };
    };
  };

  imports = [ ./common.nix ];

  config = mkIf cfg.enable {
    users = {
      users.timewarp = {
        description     = "";
        group           = "timewarp";
        createHome      = true;
        isNormalUser = true;
      };
      groups.timewarp = { };
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.timewarp = {
      description   = "";
      after         = [ "network.target" ];
      serviceConfig = {
        User = "timewarp";
        Group = "timewarp";
        Restart = "always";
        KillSignal = "SIGINT";
        PrivateTmp = true;
        ExecStart =
         let
           params = "+RTS -N -RTS --logs-prefix /home/timewarp";
           generateIP = name: value:
             if (value.config.services.timewarp-node.enable &&
                (value.config.services.timewarp-node.sender == false))
             then "--peer ${value.config.networking.publicIPv4}:${toString cfg.port}"
             else "";
         in if cfg.sender
            then pkgs.writeScript "bench-sender" ''
              #!/bin/sh
              exec ${time-warp}/bin/bench-sender \
                   --log-config ${./../static/sender-logging.yaml} \
                   -r 90 -m 10800 -d 130 \
                   ${params} ${toString (mapAttrsToList generateIP nodes)}
            ''
            else pkgs.writeScript "bench-receiver" ''
              #!/bin/sh
              exec ${time-warp}/bin/bench-receiver \
                   --log-config ${./../static/receiver-logging.yaml} \
                   -p ${toString cfg.port} -d 150 \
                   ${params}
            '';
      };
    };
  };
}
