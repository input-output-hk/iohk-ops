{ config, pkgs, lib, nodes ? null, ... }:

with (import ./../lib.nix);

let
  cfg = config.services.cardano-node;
  node0 = nodes.node0.config;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./../default.nix { inherit pkgs; }).cardano-sl-static;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  rnpDistributionParam = "(${toString cfg.genesisN},50000,${toString cfg.totalMoneyAmount},0.99)";
  smartGenIP = builtins.getEnv "SMART_GEN_IP";
  smartGenPeer =
    if (smartGenIP != "")
    then "--peer ${smartGenIP}:24962/${genDhtKey { i = 100; }}"
    else "";
  publicIP = config.networking.publicIPv4 or null;
  privateIP = config.networking.privateIPv4 or null;

  # Parse peers from a file
  #
  # > staticPeers
  # ["ip:port/dht" "ip:port/dht" ...]
  staticPeers = lib.splitString "\n" (builtins.readFile cfg.peersFile);

  # Given a list of dht/ip mappings, generate peers line
  #
  # > genPeers ["ip:port/dht" "ip:port/dht" ...]
  # "--peer ip:port/dht --peer ip:port/dht ..."
  genPeers = peers: toString (map (p: "--peer " + p) peers);

  # Given a list of NixOS configs, generate peers
  genPeersFromConfig = configs: genPeers (map (c: "${c.networking.publicIPv4}:${toString c.services.cardano-node.port}/${c.services.cardano-node.dhtKey}") configs);

  command = toString [
    cfg.executable
    "--listen ${if privateIP == null then "0.0.0.0" else privateIP}:${toString cfg.port}"
    (optionalString (publicIP != null) "--pubhost ${publicIP}")
    # Profiling
    # NB. can trigger https://ghc.haskell.org/trac/ghc/ticket/7836
    # (it actually happened)
    #"+RTS -N -pa -hb -T -A6G -qg -RTS"
    # Event logging (cannot be used with profiling)
    #"+RTS -N -T -l -A6G -qg -RTS"
    (optionalString cfg.stats "--stats")
    (optionalString (!cfg.productionMode) "--rebuild-db")
    (optionalString (!cfg.productionMode) "--spending-genesis ${toString cfg.testIndex}")
    (optionalString (!cfg.productionMode) "--vss-genesis ${toString cfg.testIndex}")
    (optionalString (cfg.distribution && !cfg.productionMode && cfg.richPoorDistr) (
       "--rich-poor-distr \"${rnpDistributionParam}\""))
    (optionalString (cfg.distribution && !cfg.productionMode && !cfg.richPoorDistr) (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    (optionalString cfg.jsonLog "--json-log ${stateDir}/jsonLog.json")
    "--dht-key ${cfg.dhtKey}"
    (optionalString cfg.productionMode "--keyfile ${stateDir}key${toString (cfg.testIndex + 1)}.sk")
    (optionalString (cfg.productionMode && cfg.systemStart != 0) "--system-start ${toString cfg.systemStart}")
    (optionalString cfg.supporter "--supporter")
    (optionalString cfg.timeLord "--time-lord")
    "--log-config ${./../static/csl-logging.yaml}"
    "--logs-prefix /var/lib/cardano-node"
    (optionalString (!cfg.enableP2P) "--explicit-initial --disable-propagation ${smartGenPeer}")
    (optionalString (!generatingAMI)
       (if (cfg.peersFile == null)
        then (if cfg.enableP2P
              then genPeersFromConfig [node0]
              else genPeersFromConfig (mapAttrsToList (name: value: value.config) (removeAttrs nodes ["report-server"]))
             )
        else genPeers staticPeers
    ))
  ];
in {
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };
      systemStart = mkOption { type = types.int; default = 0; };

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

      executable = mkOption {
        type = types.str;
        description = "Executable to run as the daemon.";
        default = "${cardano}/bin/cardano-node";
      };
      autoStart = mkOption { type = types.bool; default = false; };
      peersFile = mkOption {
        type = types.nullOr types.path;
        description = "A file with peer/dht mappings";
        default = null;
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
      richPoorDistr = mkOption {
        type = types.bool;
        default = false;
        description = "Enable rich-poor distr";
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

    # Workaround for CSL-1029
    services.cron.systemCronJobs =
    let
      # Reboot cardano-node every hour, offset by node id (in 4 minute intervals) modulo 60min
      hour = (mod (cfg.testIndex * 4) 60);
    in [
      "${toString hour} * * * * root /run/current-system/sw/bin/systemctl restart cardano-node"
    ];

    networking.firewall.allowedTCPPorts = [ cfg.port ];

    systemd.services.cardano-node = {
      description   = "cardano node service";
      after         = [ "network.target" ];
      wantedBy = optionals cfg.autoStart [ "multi-user.target" ];
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
