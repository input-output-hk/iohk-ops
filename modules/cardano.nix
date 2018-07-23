# XXX: rename this file:  cardano-node-config vs. cardano-nodes-config is CRUEL
with (import ./../lib.nix);

globals: imports: params:
  { pkgs, nodes, config, resources, options, ...}:
  let
    assetLockFile = ../static/asset-locked-addresses.txt;
    nodeNameToPublicIP   = name: let ip = nodes.${name}.config.services.cardano-node.publicIP;
                                 in if ip == null then "" else ip;
    neighbourPairs       = map (n: { name = n; ip = nodeNameToPublicIP n; })
                               (builtins.trace "${params.name}: role '${params.nodeType}'" params.peers);
    ppNeighbour          = n: "${n.name}: ${n.ip}";
    sep                  = ", ";
    cfg                  = config.services.cardano-node;
  in {
    imports = [
      (import ./cardano-service.nix globals params)
      (import ./cardano-base.nix globals imports params)
    ];

    networking.extraHosts =
    let hostList = if config.services.cardano-node.enable == false then []
                   else
                   neighbourPairs
                   ++ (map (x:
                        { name = x;
                          ip   = nodeNameToPublicIP x; })
                        (attrNames globals.nodeMap))
                   ++ (if !(hasAttr "publicIP" config.services.cardano-node) then []
                       else [ { name = params.name;
                                ip   = nodeNameToPublicIP params.name; } ]);
    in
    ''
    ${concatStringsSep "\n" (map (host: "${toString host.ip} ${host.name}.cardano") hostList)}
    '';

    services.cardano-node = {
      enable         = true;
      nodeName       = params.name;
      nodeIndex      = params.i;
      relayIndex     = params.relayIndex;
      port           = params.port;
      enablePolicies = (globals.environment == "benchmark");
      topologyYaml   =
         if !params.typeIsExplorer
         then globals.topologyYaml
         else
           let relayAddressSpecs =
             if (globals.environment == "development" || globals.environment == "benchmark")
             then map (name: { addrType = "addr"; addr = nodeNameToPublicIP name; })
                      (map (x: x.name) globals.relays)
             else map (idx:  { addrType = "host"; addr = "cardano-node-${toString idx}.${config.global.dnsDomainname}"; })
                      (range 0 (globals.nRelays - 1));
           in pkgs.writeText "topology-explorer.yaml" ''
wallet:
  relays: [[${concatStringsSep ", " (map ({ addrType, addr }: "{\"${addrType}\": \"${addr}\", \"port\": ${toString params.port}}")
                                         relayAddressSpecs)}]]
  valency: 3
  fallbacks: 2
           '';
      systemStart = params.systemStart;
      jsonLog = (globals.environment == "benchmark");
      nodeType    = params.nodeType;
      neighbours = builtins.trace "${params.name}: neighbours: ${concatStringsSep sep (map ppNeighbour neighbourPairs)}"
                                  neighbourPairs;
      publicIP = if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else null;
      privateIP = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
      statsdServer = "127.0.0.1:8125";
    } // (optionalAttrs (params.typeIsCore && (builtins.pathExists assetLockFile)) { inherit assetLockFile; });
    deployment.keys =
      (optionalAttrs (params.typeIsCore && cfg.productionMode)
        (let keyfile = "key${toString params.i}.sk";
         in {
           "key${toString params.i}" = builtins.trace (params.name + ": using " + keyfile) {
             keyFile = ./. + "/../keys/${keyfile}";
             user = "cardano-node";
           };}));
  }
