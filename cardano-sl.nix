{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardanod;
  name = "cardanod";
  stateDir = "/var/lib/cardanod/";
  srk-pkgs = import ./srk-nixpkgs/srk-pkgs.nix { inherit pkgs; };
  enableIf = cond: flag: if cond then flag else "";
in
{
  options = {
    services.routingd = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; };
    };
  };

  config = mkIf cfg.enable {
    users = {
      users.cardanod = {
        uid             = 10014;
        description     = "cardanod server user";
        group           = "cardanod";
        home            = stateDir;
        createHome      = true;
      };
      groups.cardanod = {
        gid = 123123;
      };
    };

    networking.firewall.allowedTCPPorts = [ port ];

    systemd.services.cardanod = {
      description   = "routing daemon service";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];
      serviceConfig = {
        User = "cardanod";
        Group = "cardanod";
        Restart = "always";
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
        ExecStart = toString [ ]; # TODO
      };
    };
  };
}
