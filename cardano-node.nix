{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  srk-pkgs = import ./srk-nixpkgs/srk-pkgs.nix { inherit pkgs; };
  cardano = srk-pkgs.cardano;
  discoveryPeer = "${peerHost}:${toString peerPort}/${peerDHTKey}";
  enableIf = cond: flag: if cond then flag else "";
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };
      supporter = mkOption { type = types.bool; default = false; };
      timeLord = mkOption { type = types.bool; default = false; };   
#      dhtKey = mkOption { 
#        type = types.string; 
#        description = "base64-url string describing dht key"; 
#      };
#      peerHost = mkOption { type = types.string; };
#      peerPort = mkOption { type = types.int; };
#      peerDHTKey = mkOption { type = types.string; };
    };
  };

  config = mkIf cfg.enable {
    users = {
      users.cardano-node = {
        uid             = 10014;
        description     = "cardano-node server user";
        group           = "cardano-node";
        home            = stateDir;
        createHome      = true;
      };
      groups.cardano-node = {
        gid = 123123;
      };
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.cardano-node = {
      description   = "routing daemon service";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "always";
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
        ExecStart = toString [ 
	  "${cardano}/bin/pos-node"
          "--port ${toString cfg.port}"
#          "--peer ${discoveryPeer}"
          (enableIf cfg.supporter "--supporter")
          (enableIf cfg.timeLord "--time-lord")
        ]; 
      };
    };
  };
}
