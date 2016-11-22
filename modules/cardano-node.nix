{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN slotDuration networkDiameter mpcRelayInterval; }).cardano-sl;
  discoveryPeer = "${cfg.peerHost}:${toString cfg.peerPort}/${cfg.peerDhtKey}";
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  enableIf = cond: flag: if cond then flag else "";
  command = toString [
    "${cardano}/bin/cardano-node"
    "--port ${toString cfg.port}"
    "--rebuild-db"
    "+RTS -N -RTS"
    (enableIf cfg.stats "--stats")
    "--spending-genesis ${toString cfg.testIndex}"
    "--vss-genesis ${toString cfg.testIndex}"
    (enableIf cfg.distribution (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    (enableIf cfg.peerEnable "--peer ${discoveryPeer}")
    (enableIf cfg.jsonLog "--json-log ${stateDir}/jsonLog.json")
    (enableIf (! cfg.peerEnable) "--dht-key ${cfg.dhtKey}")
    (enableIf cfg.supporter "--supporter")
    (enableIf cfg.timeLord "--time-lord")
    "--log-config /var/lib/cardano-node/logging.yaml"
    "--logs-prefix /var/lib/cardano-node"
  ];
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };

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
      networkDiameter = mkOption { type = types.int; };
      mpcRelayInterval = mkOption { type = types.int; };

      stats = mkOption { type = types.bool; default = false; };
      jsonLog = mkOption { type = types.bool; default = false; };
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
        ExecStart = "/bin/sh -c '${command}'";
      };
    };
  };
}
