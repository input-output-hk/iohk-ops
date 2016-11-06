{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN; }).cardano-sl;
  discoveryPeer = "${cfg.peerHost}:${toString cfg.peerPort}/${cfg.peerDhtKey}";
  enableIf = cond: flag: if cond then flag else "";
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };
      isDebug = mkOption { type = types.int; default = false; };
      supporter = mkOption { type = types.bool; default = false; };
      timeLord = mkOption { type = types.bool; default = false; };   
      dhtKey = mkOption { 
        type = types.string; 
        description = "base64-url string describing dht key"; 
      };
      genesisN = mkOption { type = types.int; };
      testIndex = mkOption { type = types.int; };
      pettyUtxo = mkOption { type = types.bool; default = false; };

      peerEnable = mkOption { type = types.bool; default = true;};
      peerHost = mkOption { type = types.string; };
      peerPort = mkOption { type = types.int; default = cfg.port; };
      peerDhtKey = mkOption { type = types.string; };
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
      description   = "cardano node service";
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
          "/bin/sh -c '"
	  "${cardano}/bin/cardano-node"
          "--port ${toString cfg.port}"
          "--rebuild-db"
          "--spending-genesis ${toString cfg.testIndex}"
          "--vss-genesis ${toString cfg.testIndex}"
          (enableIf cfg.pettyUtxo "--petty-utxo")
          (enableIf cfg.peerEnable "--peer ${discoveryPeer}")
          (enableIf (! cfg.peerEnable) "--dht-key ${cfg.dhtKey}")
          (enableIf cfg.supporter "--supporter")
          (if cfg.isDebug 
           then "--main-log Debug"
           else "--main-log Info")
          (enableIf cfg.timeLord "--time-lord")
          " > ${stateDir}/cardano-node.log 2>&1'"
        ]; 
      };
    };
  };
}
