{ config, pkgs, lib, nodes, ... } :

with (import ./../lib.nix);

let
  cfg = config.services.cardano-node;
  node0 = nodes.node0.config;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./../srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN slotDuration networkDiameter mpcRelayInterval; }).cardano-sl;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  enableIf = cond: flag: if cond then flag else "";
  smartGenIP = builtins.getEnv "SMART_GEN_IP";
  smartGenPeer =
    if (smartGenIP != "")
    then "--peer ${smartGenIP}:24962/${genDhtKey 999}"
    else "";

  command = toString [
    "${cardano}/bin/cardano-node"
    "--port ${toString cfg.port}"
    "--rebuild-db"
    # Profiling
    "+RTS -N -pa -hb -T -A6G -qg -RTS"
    # Event logging (cannot be used with profiling)
    #"+RTS -N -T -l -A6G -qg -RTS"
    (enableIf cfg.stats "--stats")
    "--spending-genesis ${toString cfg.testIndex}"
    "--vss-genesis ${toString cfg.testIndex}"
    (enableIf cfg.distribution (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    (enableIf cfg.jsonLog "--json-log ${stateDir}/jsonLog.json")
    "--dht-key ${cfg.dhtKey}"
    (enableIf cfg.supporter "--supporter")
    (enableIf cfg.timeLord "--time-lord")
    "--memory-mode" #add option to nixops.nix
    "--log-config ${./../static/csl-logging.yaml}"
    "--logs-prefix /var/lib/cardano-node"
    (if cfg.enableP2P
       then "--peer ${node0.networking.publicIPv4}:${toString node0.services.cardano-node.port}/${node0.services.cardano-node.dhtKey} ${smartGenPeer}"
       else (toString (mapAttrsToList (name: value: "--peer ${value.config.networking.publicIPv4}:${toString value.config.services.cardano-node.port}/${value.config.services.cardano-node.dhtKey}") nodes))
    )
  ];
in
{
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };

      enableP2P = mkOption { type = types.bool; default = false; };
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
