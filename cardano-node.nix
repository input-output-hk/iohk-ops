{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN slotDuration; }).cardano-sl;
  discoveryPeer = "${cfg.peerHost}:${toString cfg.peerPort}/${cfg.peerDhtKey}";
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  enableIf = cond: flag: if cond then flag else "";
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };
      stats = mkOption { type = types.bool; default = false; };

      # such a shame
      isDebug = mkOption { type = types.bool; default = false; };
      isInfo = mkOption { type = types.bool; default = false; };
      isError = mkOption { type = types.bool; default = false; };

      supporter = mkOption { type = types.bool; default = false; };
      timeLord = mkOption { type = types.bool; default = false; };   
      dhtKey = mkOption { 
        type = types.string; 
        description = "base64-url string describing dht key"; 
      };

      genesisN = mkOption { type = types.int; };
      slotDuration = mkOption { type = types.int; };
      pettyUtxo = mkOption { type = types.bool; default = false; };
      totalMoneyAmount = mkOption { type = types.int; default = 100000; };
      distribution = mkOption { 
        type = types.bool; 
        default = true; 
        description = "pass distribution flag"; 
      };
      bitcoinOverFlat = mkOption { 
        type = types.bool; 
        default = false;
        description = "If distribution is on, use bitcoin distribution. Otherwise flat";
      };

      testIndex = mkOption { type = types.int; };

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
          (enableIf cfg.stats "--stats")
          "--spending-genesis ${toString cfg.testIndex}"
          "--vss-genesis ${toString cfg.testIndex}"
          (enableIf cfg.distribution (
             if cfg.bitcoinOverFlat
             then "--bitcoin-distr \"${distributionParam}\""
             else "--flat-distr \"${distributionParam}\""))
          (enableIf cfg.pettyUtxo "--petty-utxo")
          (enableIf cfg.peerEnable "--peer ${discoveryPeer}")
          (enableIf (! cfg.peerEnable) "--dht-key ${cfg.dhtKey}")
          (enableIf cfg.supporter "--supporter")
          (enableIf cfg.isDebug "--main-log Debug")
          (enableIf cfg.isInfo "--main-log Info")
          (enableIf cfg.isError "--main-log Error")
          (enableIf cfg.timeLord "--time-lord")
          " > ${stateDir}/cardano-node.log 2>&1'"
        ]; 
      };
    };
  };
}
