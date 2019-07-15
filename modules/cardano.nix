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

  in {
    imports = [
      ./cardano-service.nix
      (sources.cardano-node + "/nix/nixos/cardano-node.nix")
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

    services.cardano-node = if (config.params.nodeImpl != "haskell") then {} else {
      enable = true;
      # system-start-time = config.global.systemStart; TODO
      consensus-protocol = "real-pbft"; # FIXME: should this be configurable in topology?
      slot-duration = 20;
      host = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
      port = config.params.port;
      node-id = config.params.i;
      topology = pkgs.writeText "topology.json" (builtins.toJSON (lib.mapAttrsToList (name: node: {
        nodeId = node.i;
        nodeAddress = {
          addr = if (node.i == cfg.node-id) 
            then cfg.host
            else (nodeNameToPublicIP name);
          port = toString node.port;
        };
        producers = if (node.i == cfg.node-id) 
          then map (n: {
            addr = n.ip;
            port = toString config.global.nodeMap.${n.name}.port;
          }) (builtins.filter (n: config.global.nodeMap.${n.name}.typeIsCore) neighbourPairs)
          else [];
      }) config.global.nodeMap));
      logger.config-file = ./iohk-monitoring-config.yaml;
    };

    networking.firewall = mkIf cfg.enable {
      allowedTCPPorts = [ cfg.port ];
    };

    services.cardano-node-legacy = if (config.params.nodeImpl != "legacy") then {} else {
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
      (optionalAttrs (config.params.typeIsCore && cfgLegacy.enable && cfgLegacy.productionMode)
        (let keyfile = "key${toString config.params.i}.sk";
         in {
           "key${toString config.params.i}" = builtins.trace (config.params.name + ": using " + keyfile) {
             keyFile = ./. + "/../keys/${keyfile}";
             user = "cardano-node";
           };}));
  }
