# XXX: rename this file:  cardano-node-config vs. cardano-nodes-config is CRUEL
with import ../lib.nix;

{ pkgs, nodes, config, resources, options, lib, ...}:

  let
    assetLockFile = ../static/asset-locked-addresses.txt;
    nodeNameToPublicIP   = name:
      if nodes.${name}.options.networking.publicIPv4.isDefined
      then nodes.${name}.config.networking.publicIPv4 else "";
    neighbourPairs       = map (n: { name = n; ip = nodeNameToPublicIP n; })
                               (builtins.trace "${config.params.name}: role '${config.params.nodeType}'" config.params.peers);
    ppNeighbour          = n: "${n.name}: ${n.ip}";
    sep                  = ", ";
    cfgLegacy            = config.services.cardano-node-legacy;
    cfg                  = config.services.cardano-node;
    sources = import ../nix/sources.nix;
    toCardanoEnvName = env: {
      # mapping of environnement name from globals.nix to the one defined in cardanoLib:
      stagingshelleyshort = "shelley_staging_short";
      stagingshelley      = "shelley_staging";
    }.${env} or env;
  in {
    imports = [
      ./cardano-service.nix
      (sources.cardano-node + "/nix/nixos")
      (sources.cardano-byron-proxy + "/nix/nixos")
      ./cardano-base.nix
      ./globals.nix
    ];

    networking.extraHosts =
    let hostList = if (config.services.cardano-node.enable || config.services.cardano-node-legacy.enable)
                   then (neighbourPairs
                   ++ (map (x:
                        { name = x;
                          ip   = nodeNameToPublicIP x; })
                        (attrNames config.global.nodeMap))
                   ++ (if (hasAttr "publicIP" config.services.cardano-node || hasAttr "host" config.services.cardano-node)
                       then [ { name = config.params.name;
                                ip   = nodeNameToPublicIP config.params.name; } ]
                       else []))
                   else [];
    in
    ''
    ${concatStringsSep "\n" (map (host: "${toString host.ip} ${host.name}.cardano") hostList)}
    '';

    services.byron-proxy = if (config.params.nodeImpl != "haskell") then {} else {
      enable = true;
      environment = toCardanoEnvName config.global.environment;
      topologyFile = config.global.topologyYaml;
      nodeId = config.params.name;
      pbftThreshold = "0.9";
      address = "${if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else "127.0.0.1"}:${toString config.params.port}";
      listen = "0.0.0.0:${toString config.params.port}";
      proxyHost = "0.0.0.0";
    };

    services.cardano-node = mkIf (config.params.nodeImpl == "haskell") {
      enable = true;
      pbftThreshold = "0.9";
      inherit (cardanoLib.environments.${toCardanoEnvName config.global.environment})
        genesisFile
        genesisHash;
      signingKey = if (config.params.typeIsCore)
        then "/var/lib/keys/cardano-node"
        else null;
      # delegation-certificate = TODO;
      consensusProtocol = "real-pbft";
      hostAddr = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
      port = config.params.port + 1;
      nodeId = config.params.i;
      topology = pkgs.writeText "topology.json" (builtins.toJSON (lib.mapAttrsToList (name: node: {
        nodeId = node.i;
        nodeAddress = {
          addr = if (node.i == cfg.nodeId)
            then cfg.hostAddr
            else (nodeNameToPublicIP name);
          port = node.port + 1;
        };
        producers = if (node.i == cfg.nodeId)
          then (map (n: {
            addr = n.ip;
            port = config.global.nodeMap.${n.name}.port + 1;
            valency = 1;
          }) neighbourPairs
          ++ [{
            addr = "127.0.0.1";
            port = 7777;
            valency = 1;
          }])
          else [];
      }) config.global.nodeMap));
      logger.configFile = ./iohk-monitoring-config.yaml;
    };

    networking.firewall = mkIf cfg.enable {
      allowedTCPPorts = [ cfg.port (cfg.port + 1) ];
    };

    systemd.services."cardano-node" = mkIf (cfg.enable && config.params.typeIsCore) {
      after = [ "cardano-node-key.service" ];
      wants = [ "cardano-node-key.service" ];
    };

    services.cardano-node-legacy = mkIf (config.params.nodeImpl == "legacy") {
      enable         = true;
      nodeName       = config.params.name;
      nodeIndex      = config.params.i;
      relayIndex     = params.relayIndex;
      port           = config.params.port;
      enablePolicies = config.global.environment == "benchmark";
      topologyYaml   =
         if !config.params.typeIsExplorer
         then config.global.topologyYaml
         else
           let relayAddressSpecs =
             if (config.global.environment == "development" || config.global.environment == "benchmark")
             then map (name: { addrType = "addr"; addr = nodeNameToPublicIP name; })
                      (map (x: x.name) config.global.relays)
             else map (idx:  { addrType = "host"; addr = "cardano-node-${toString idx}.${config.global.dnsDomainname}"; })
                      (range 0 (config.global.nRelays - 1));
           in pkgs.writeText "topology-explorer.yaml" ''
wallet:
  relays: [[${concatStringsSep ", " (map ({ addrType, addr }: "{\"${addrType}\": \"${addr}\", \"port\": ${toString config.params.port}}")
                                         relayAddressSpecs)}]]
  valency: 3
  fallbacks: 2
           '';
      systemStart = params.systemStart;
      jsonLog = (config.global.environment == "benchmark");
      nodeType    = config.params.nodeType;
      neighbours = builtins.trace "${config.params.name}: neighbours: ${concatStringsSep sep (map ppNeighbour neighbourPairs)}"
                                  neighbourPairs;
      publicIP = if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else null;
      privateIP = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
      statsdServer = "127.0.0.1:8125";
    } // (optionalAttrs (config.params.typeIsCore && (builtins.pathExists assetLockFile)) { inherit assetLockFile; });

    deployment.keys =
      (optionalAttrs (config.params.typeIsCore && ((cfgLegacy.enable && cfgLegacy.productionMode) || cfg.enable)) (
        let keyfile = "key${toString config.params.i}.sk"; in
        {
          "cardano-node" = builtins.trace (config.params.name + ": using " + keyfile) {
              keyFile = ./. + "/../keys/${keyfile}";
              user = "cardano-node";
              destDir = "/var/lib/keys";
          };
        }
      ));

    systemd.services."cardano-node-legacy" = mkIf (config.params.typeIsCore && cfgLegacy.enable && cfgLegacy.productionMode) {
      after = [ "cardano-node-key.service" ];
      wants = [ "cardano-node-key.service" ];
    };
  }
