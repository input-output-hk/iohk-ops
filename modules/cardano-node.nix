{ config, pkgs, lib, ... }@args:

with (import ./../lib.nix);

let
  cfg = config.services.cardano-node;
  node0 = args.nodes.node0.config;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./../srk-nixpkgs/default.nix { inherit pkgs; inherit (cfg) genesisN slotDuration networkDiameter mpcRelayInterval; }).cardano-sl;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  enableIf = cond: flag: if cond then flag else "";
  smartGenIP = builtins.getEnv "SMART_GEN_IP";
  smartGenPeer =
    if (smartGenIP != "")
    then "--peer ${smartGenIP}:24962/${genDhtKey { i = 999; }}"
    else "";

  command = toString [
    "${cardano}/bin/cardano-node"
    "--listen ${config.networking.publicIPv4 or "0.0.0.0"}:${toString cfg.port}"
    (enableIf (!cfg.productionMode) "--rebuild-db")
    # Profiling
    # NB. can trigger https://ghc.haskell.org/trac/ghc/ticket/7836
    # (it actually happened)
    #"+RTS -N -pa -hb -T -A6G -qg -RTS"
    # Event logging (cannot be used with profiling)
    #"+RTS -N -T -l -A6G -qg -RTS"
    (enableIf cfg.stats "--stats")
    (enableIf (!cfg.productionMode) "--spending-genesis ${toString cfg.testIndex}")
    (enableIf (!cfg.productionMode) "--vss-genesis ${toString cfg.testIndex}")
    (enableIf (cfg.distribution && !cfg.productionMode) (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    (enableIf cfg.jsonLog "--json-log ${stateDir}/jsonLog.json")
    "--dht-key ${cfg.dhtKey}"
    (enableIf cfg.productionMode "--keyfile ${stateDir}key${toString (cfg.testIndex + 1)}.sk")
    (enableIf cfg.supporter "--supporter")
    (enableIf cfg.timeLord "--time-lord")
    "--log-config ${./../static/csl-logging.yaml}"
    "--logs-prefix /var/lib/cardano-node"
    (enableIf (!cfg.enableP2P) "--explicit-initial --disable-propagation ${smartGenPeer}")
    (enableIf (!generatingAMI) (if cfg.enableP2P
       then "--peer ${node0.networking.publicIPv4}:${toString node0.services.cardano-node.port}/${node0.services.cardano-node.dhtKey}"
       else (toString (mapAttrsToList (name: value: "--peer ${value.config.networking.publicIPv4}:${toString value.config.services.cardano-node.port}/${value.config.services.cardano-node.dhtKey}") nodes))
    ))
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
      productionMode = mkOption {
        type = types.bool;
        default = false;
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
        extraGroups     = [ "keys" ];
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
#        Restart = "always";
        StartLimitInterval=0;
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
        ExecStart = let
            keyId = "key" + toString (cfg.testIndex + 1);
            key = keyId + ".sk";
          in pkgs.writeScript "cardano-node.sh" ''
          #!/bin/sh
          [ -f /run/keys/${keyId} ] && cp /run/keys/${keyId} ${stateDir}${key}
          exec ${command}
        '';
      };
    };
  };
}
