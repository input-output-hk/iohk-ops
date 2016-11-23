{ config, pkgs, lib, nodes, ...}: 

with lib;

let
  time-warp = (import ./../srk-nixpkgs/default.nix { inherit pkgs; }).time-warp;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
  cfg = config.services.timewarp-node;
in {

  options = {
    services.timewarp-node = {
      enable = mkEnableOption "timewarp-node";
      sender = mkOption { type = types.bool; default = false; };
    };
  };

  config = mkIf cfg.enable {
    imports = [ (import ./common.nix) ];

    users = {
      users.timewarp = {
        description     = "";
        group           = "timewarp";
        createHome      = true;
        isNormalUser = true;
      };
      groups.timewarp = { };
    };

    networking.firewall.allowedTCPPorts = [ 3055 ];

    systemd.services.timewarp = {
      description   = "";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];
      serviceConfig = {
        User = "timewarp";
        Group = "timewarp";
        Restart = "always";
        KillSignal = "SIGINT";
        PrivateTmp = true;
        ExecStart =
         let
           params = [
             "+RTS -N -RTS "
             "--logs-prefix /home/timewarp "
           ];
           generateIP = name: value:
             if (value.config.services.timewarp-node.enable &&
                (value.config.services.timewarp-node.sender == false))
             then "--peer ${value.config.deployment.ec2.elasticIPv4}"
             else "";
         in 
            if cfg.sender
            then pkgs.writeScript "bench-sender" (toString ([
                   "${time-warp}/bin/bench-sender "
                   "--log-config ${./../static/sender-logging.yaml} "
                   "-r 90 -m 10800 -d 130"
                 ] ++ params ++ (mapAttrsToList generateIP nodes)))
            else pkgs.writeScript "bench-receiver" (toString ([
                   "${time-warp}/bin/bench-receiver "
                   "--log-config ${./../static/receiver-logging.yaml} "
                   "-p 3055 -d 150 "
                 ] ++ params));
      };
    };
  };
} 
