{ config, pkgs, lib, ... } :

with lib;

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./../srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN slotDuration networkDiameter mpcRelayInterval; }).cardano-sl;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  enableIf = cond: flag: if cond then flag else "";
  command = toString [
    "${cardano}/bin/cardano-node"
    "--port ${toString cfg.port}"
    "--rebuild-db"
    "+RTS -N -pa -A6G -qg -RTS"
    (enableIf cfg.stats "--stats")
    "--spending-genesis ${toString cfg.testIndex}"
    "--vss-genesis ${toString cfg.testIndex}"
    (enableIf cfg.distribution (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    (enableIf cfg.peerEnable "--peer ${cfg.peerHost}:${toString cfg.peerPort}/${cfg.peerDhtKey}")
    (enableIf cfg.jsonLog "--json-log ${stateDir}/jsonLog.json")
    (enableIf (! cfg.peerEnable) "--dht-key ${cfg.dhtKey}")
    (enableIf cfg.supporter "--supporter")
    (enableIf cfg.timeLord "--time-lord")
    "--memory-mode" #add option to nixops.nix
    "--log-config ${./../static/csl-logging.yaml}"
    "--logs-prefix /var/lib/cardano-node"
  ];
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };

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
      peerHost = mkOption { type = types.string; default = ""; };
      peerPort = mkOption { type = types.int; default = cfg.port; };
      peerDhtKey = mkOption { type = types.string; default = ""; };
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

    environment.systemPackages = [ cardano ];

    systemd.services.cardano-node = {
      description   = "cardano node service";
      after         = [ "network.target" ];
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "always";
        StartLimitInterval=0;
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
        ExecStart = pkgs.writeScript "cardano-node.sh" ''
          #!/bin/sh
          exec ${command}
        '';
      };
    };
  };
}
