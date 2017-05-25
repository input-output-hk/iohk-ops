{ config, pkgs, lib, ... }:

with (import ./../lib.nix);

let
  cfg = config.services.cardano-node;
  name = "cardano-node";
  stateDir = "/var/lib/cardano-node/";
  cardano = (import ./../default.nix { inherit pkgs; }).cardano-sl-static;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  rnpDistributionParam = "(${toString cfg.genesisN},50000,${toString cfg.totalMoneyAmount},0.99)";
  smartGenIP = builtins.getEnv "SMART_GEN_IP";
  smartGenPeer =
    if (smartGenIP != "")
    then "--peer ${smartGenIP}:24962/${genDhtKey 100 }}"
    else "";
  publicIP = config.networking.publicIPv4 or null;
  privateIP = config.networking.privateIPv4 or null;

  # Given a list of dht/ip mappings, generate peers line
  #
  # > genPeers ["ip:port/dht" "ip:port/dht" ...]
  # "--peer ip:port/dht --peer ip:port/dht ..."
  genPeers = peers: toString (map (p: "--peer " + p) peers);

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
    "--log-config ${./../static/csl-logging.yaml}"
    "--logs-prefix /var/lib/cardano-node"
    (optionalString (!cfg.enableP2P) "--explicit-initial --disable-propagation ${smartGenPeer}")
    (genPeers cfg.initialPeers)
  ];
in {
  options = {
    services.cardano-node = {
      enable = mkEnableOption name;
      port = mkOption { type = types.int; default = 3000; };
      systemStart = mkOption { type = types.int; default = 0; };

      enableP2P = mkOption { type = types.bool; default = false; };
      supporter = mkOption { type = types.bool; default = false; };
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
      initialPeers = mkOption {
        type = types.nullOr (types.listOf types.str);
        description = "A file with peer/dht mappings";
        default = null;
      };

      genesisN = mkOption { type = types.int; };
      slotDuration = mkOption { type = types.int; };
      networkDiameter = mkOption { type = types.int; };
      mpcRelayInterval = mkOption { type = types.int; };

      stats = mkOption { type = types.bool; default = false; };
      jsonLog = mkOption { type = types.bool; default = true; };
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
    assertions = [{
      assertion = cfg.initialPeers != null;
      message = "services.cardano-node.initialPeers must be set, a node needs at least one initial peer (testIndex: ${toString cfg.testIndex})";
    }];

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

    services.cardano-node.dhtKey = mkDefault (genDhtKey cfg.testIndex);

    # Workaround for CSL-1029
    services.cron.systemCronJobs =
    let
      # Reboot cardano-node every hour, offset by node id (in 4 minute intervals) modulo 60min
      hour = (mod (cfg.testIndex * 4) 60);
    in [
      "${toString hour} * * * * root /run/current-system/sw/bin/systemctl restart cardano-node"
    ];

    networking.firewall = {
      allowedTCPPorts = [ cfg.port ];

      # TODO: securing this depends on CSLA-27
      # NOTE: this implicitly blocks DHCPCD, which uses port 68
      allowedUDPPortRanges = [ { from = 1024; to = 65000; } ];
    };

    systemd.services.cardano-node = {
      description   = "cardano node service";
      after         = [ "network.target" ];
      wantedBy = optionals cfg.autoStart [ "multi-user.target" ];
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        # Allow a maximum of 5 retries separated by 30 seconds, in total capped by 200s
        Restart = "always";
        RestartSec = 30;
        StartLimitInterval = 200;
        StartLimitBurst = 5;
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
