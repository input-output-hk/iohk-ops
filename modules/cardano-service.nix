{ config, pkgs, lib, options, ... }:
with builtins; with lib;
let
  cfg = config.services.cardano-node-legacy;

  systemdServiceName = "cardano-node-legacy${optionalString cfg.instanced "@"}";
  stateDir = "/var/lib/cardano-node";
  stateDirFull
            = if cfg.instanced
              then "${stateDir}/$nodeId"
              else stateDir;
  privateIP = if cfg.instanced
              then "10.1.0.$((1 + $1))"
              else cfg.privateIP;
  port      = if cfg.instanced
              then toString cfg.port #"$((${toString cfg.port} + $1))"
              else toString cfg.port;
  nodeId    = if cfg.instanced
              then "$nodeId"
              else cfg.name;
  keySrc    = if cfg.instanced
              then "/var/lib/keys/cardano-node/key$1.sk"
              else "/var/lib/keys/cardano-node";
  keyFile   = if cfg.instanced
              then "${stateDirFull}/key$1.sk"
              else "${stateDirFull}/key${toString cfg.nodeIndex}.sk";

  iohkPkgs = import ../default.nix { };
  cardano = iohkPkgs.nix-tools.cexes.cardano-sl-node.cardano-node-simple;
  cardano-config = ../../cardano-sl;
  distributionParam = "(${toString cfg.genesisN},${toString cfg.totalMoneyAmount})";
  rnpDistributionParam = "(${toString cfg.genesisN},50000,${toString cfg.totalMoneyAmount},0.99)";
  smartGenIP = builtins.getEnv "SMART_GEN_IP";

  command = toString [
    cfg.executable
    (optionalString (cfg.publicIP != null && cfg.name != "explorer")
     "--address ${cfg.publicIP}:${port}")
    (optionalString (cfg.name != "explorer")
     "--listen ${privateIP}:${port}")
    # Profiling
    # NB. can trigger https://ghc.haskell.org/trac/ghc/ticket/7836
    # (it actually happened)
    #"+RTS -N -pa -hb -T -A6G -qg -RTS"
    # Event logging (cannot be used with profiling)
    #"+RTS -N -T -l -A6G -qg -RTS"
    (optionalString cfg.stats "--stats")
    (optionalString (!cfg.productionMode) "--rebuild-db")
    (optionalString (!cfg.productionMode) "--spending-genesis ${toString cfg.nodeIndex}")
    (optionalString (!cfg.productionMode) "--vss-genesis ${toString cfg.nodeIndex}")
    (optionalString (cfg.distribution && !cfg.productionMode && cfg.richPoorDistr) (
       "--rich-poor-distr \"${rnpDistributionParam}\""))
    (optionalString (cfg.distribution && !cfg.productionMode && !cfg.richPoorDistr) (
       if cfg.bitcoinOverFlat
       then "--bitcoin-distr \"${distributionParam}\""
       else "--flat-distr \"${distributionParam}\""))
    # (optionalString cfg.jsonLog "--json-log ${stateDirFull}/jsonLog.json")
    (optionalString (cfg.statsdServer != null) "--metrics +RTS -T -RTS --statsd-server ${cfg.statsdServer}")
    (optionalString (cfg.serveEkg)             "--ekg-server ${cfg.privateIP}:8080")
    (optionalString (cfg.productionMode && cfg.name != "explorer")
      "--keyfile ${keyFile}")
    (optionalString (cfg.productionMode && cfg.systemStart != 0) "--system-start ${toString cfg.systemStart}")
    (optionalString cfg.supporter "--supporter")
    "--log-config ${cardano-config}/log-configs/cluster.yaml"
    "--logs-prefix ${stateDirFull}"
    "--db-path ${stateDirFull}/node-db"
    (optionalString (!cfg.enableP2P) "--kademlia-explicit-initial --disable-propagation ${smartGenPeer}")
    "--configuration-file ${cardano-config}/lib/configuration.yaml"
    "--configuration-key ${config.deployment.arguments.configurationKey}"
    "--topology ${cfg.topologyYaml}"
    (optionalString (cfg.assetLockFile != null) "--asset-lock-file ${cfg.assetLockFile}")
    "--node-id ${nodeId}"
    (optionalString cfg.enablePolicies ("--policies " + (if cfg.nodeType == "core" then "${../benchmarks/policy_core.yaml}" else "${../benchmarks/policy_relay.yaml}")))
    (optionalString cfg.enableProfiling "+RTS -p -RTS")
  ];
in {
  options = {
    services.cardano-node-legacy = {
      name = mkOption { type = types.str; };

      instanced = mkOption { type = types.bool; default = false; };
      names = mkOption { type = types.listOf types.str; default = []; };
      
      enable = mkEnableOption "cardano-node-legacy";
      port = mkOption { type = types.int; default = 3000; };
      systemStart = mkOption { type = types.int; default = 0; };

      preseedDB = mkOption { type = types.nullOr types.path; default = null; };
      
      enableP2P = mkOption { type = types.bool; default = true; };
      supporter = mkOption { type = types.bool; default = false; };
      enableProfiling = mkOption { type = types.bool; default = false; };
      enablePolicies = mkOption { type = types.bool; default = false; };
      productionMode = mkOption {
        type = types.bool;
        default = true;
      };
      saveCoreDumps = mkOption {
        type = types.bool;
        default = true;
        description = "automatically save coredumps when cardano-node segfaults";
      };

      executable = mkOption {
        type = types.str;
        description = "Executable to run as the daemon.";
        default = "${cardano}/bin/cardano-node-simple";
      };
      autoStart = mkOption { type = types.bool; default = ! cfg.instanced; };

      topologyYaml = mkOption { type = types.path; };
      assetLockFile = mkOption { type = types.nullOr types.path; default = null; };

      genesisN = mkOption { type = types.int; default = 6; };
      slotDuration = mkOption { type = types.int; default = 20; };
      networkDiameter = mkOption { type = types.int; default = 15; };
      mpcRelayInterval = mkOption { type = types.int; default = 45; };
      stats = mkOption { type = types.bool; default = false; };
      jsonLog = mkOption { type = types.bool; default = true; };
      totalMoneyAmount = mkOption { type = types.int; default = 60000000; };
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

      nodeIndex  = mkOption { type = types.int; };
      relayIndex = mkOption { type = types.int; };
      nodeName   = mkOption { type = types.str; };
      nodeType   = mkOption { type = types.enum [ "core" "relay" "other" ]; default = null; };

      publicIP = mkOption {
        type = types.nullOr types.str;
        description = "Public IP to advertise to peers";
        default = null;
      };

      privateIP = mkOption {
        type = types.str;
        description = "Private IP to bind to";
        default = "0.0.0.0";
      };

      neighbours = mkOption {
        default = [];
        type = types.listOf types.str;
        description = ''List of name:ip pairs of neighbours.'';
      };

      statsdServer = mkOption {
        type = types.nullOr types.str;
        description = "IP:Port of the EKG telemetry sink";
        default = null;
      };

      serveEkg = mkOption {
        type = types.bool;
        description = "Serve EKG web UI on port 8080";
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
    { assertion = cfg.nodeType != null;
      message = "services.cardano-node-legacy.type must be set to one of: core relay other"; }
    ];

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

    networking.firewall = {
      allowedTCPPorts = [ cfg.port ] ++ optionals cfg.serveEkg [ 8080 ];

      # TODO: securing this depends on CSLA-27
      # NOTE: this implicitly blocks DHCPCD, which uses port 68
      allowedUDPPortRanges = [ { from = 1024; to = 65000; } ];
    };

    # Workaround for CSL-1320
    #systemd.services.cardano-restart = let
    #  # how many minutes between nodes restarting
    #  nodeMinute = mod (cfg.nodeIndex * 4) 60;
    #in {
    #  script = (if (config.deployment.arguments.configurationKey == "bench")
    #    then '' echo System not restarted because benchmark is running.''
    #    else '' /run/current-system/sw/bin/systemctl restart cardano-node-legacy'');
    #  # Reboot cardano-node every 36h (except Mon->Tue gap which is 24h)
    #  startAt = [
    #    "Tue,Fri,Mon 13:${toString nodeMinute}"
    #    "Thu,Sun     01:${toString nodeMinute}"
    #  ];
    #};

    systemd.services."${systemdServiceName}" = {
      description   = "cardano node service";
      after         = [ "network.target" ];
      wantedBy = optionals cfg.autoStart [ "multi-user.target" ];
      scriptArgs = "%i";
      script = let
        key = "key${toString cfg.nodeIndex}.sk";
      in ''
        set -x
        choice() { i=$1; shift; eval "echo \''${$((i + 1))}"; }
        nodeId=$(choice $1 ${concatStringsSep " " cfg.names})
        if test ! -d ${stateDirFull}/node-db
        then mkdir -p            ${stateDirFull}
        '' + optionalString (cfg.preseedDB != null) ''
             cp ${cfg.preseedDB} ${stateDirFull}/node-db -r
             chown cardano-node  ${stateDirFull}/node-db -R
             du --max-depth=1 -h ${stateDirFull}/node-db          | systemd-cat
             echo "=============================================" | systemd-cat
             echo "  initialised ${stateDirFull}"                 | systemd-cat
             echo "=============================================" | systemd-cat
        '' + ''
             chmod u+w           ${stateDirFull} -R
             chmod go-rwx        ${stateDirFull} -R
        fi
        [ -f ${keySrc} ] && cp -af ${keySrc} ${keyFile}
        ${optionalString (cfg.saveCoreDumps) ''
          # only a process with non-zero coresize can coredump (the default is 0)
          ulimit -c unlimited
        ''}
        exec ${command}
      '';
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
        Type = "notify";
      };
    };

    systemd.services.cardano-node-legacy-recorder = mkIf (config.deployment.arguments.configurationKey == "bench") {
      description   = "recording metrics on cardano node service";
      after         = [ "systemd.services.cardano-node-legacy" ];
      wantedBy = optionals cfg.autoStart [ "multi-user.target" ];
      path = [ pkgs.glibc pkgs.procps ];  # dependencies
      script = ''
        ${../benchmarks/scripts/record-stats.sh} -exec ${cfg.executable} >> "${stateDirFull}/time-slave.log"
      '';
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
      };
    };
  };
}
