# TODO: get rid of this duplication between config.nix and modules/cardano-node.nix
# XXX: rename this file:  cardano-node-config vs. cardano-nodes-config is CRUEL
with (import ./../lib.nix);

globals: imports: params:
  { pkgs, nodes, config, resources, options, ...}:
  let
    nodeNameToPublicIP   = name: nodes.${name}.config.services.cardano-node.publicIP;
    neighbourPairs       = map (n: { name = n; ip = nodeNameToPublicIP n; })
                               (builtins.trace "${params.name}: role '${params.nodeType}'" params.peers);
    ppNeighbour          = n: "${n.name}: ${n.ip}";
    sep                  = ", ";
    cfg                  = config.services.cardano-node;
  in {
    # _module.args.globals = globals; ## Important:  pass through globals

    imports = [
      ./common.nix
      (import ./cardano-node.nix globals params)
      (import ./amazon-base.nix  globals params)
    ] ++ map (path: import path globals params) imports;

    services.dnsmasq.enable = true;
    services.dnsmasq.servers = [ "127.0.0.1" ];

    # TODO: DEVOPS-8
    #deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
    deployment.ec2.region         = mkForce params.region;
    deployment.ec2.accessKeyId    = params.accessKeyId;
    deployment.ec2.keyPair        = resources.ec2KeyPairs.${params.keyPairName};
    deployment.ec2.securityGroups = mkForce (map (x: resources.ec2SecurityGroups.${x}) params.sgNames);
    
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
      enable      = true;
      nodeName    = params.name;
      nodeIndex   = params.i;
      relayIndex  = params.relayIndex;
      port        = params.port;
      systemStart = params.systemStart;
      nodeType    = params.nodeType;
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval;
      inherit (cconf) totalMoneyAmount bitcoinOverFlat productionMode richPoorDistr;
      neighbours = builtins.trace "${params.name}: neighbours: ${concatStringsSep sep (map ppNeighbour neighbourPairs)}"
                                  neighbourPairs;
      publicIP = if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else null;
      privateIP = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
      statsdServer = "127.0.0.1:8125";
    };

    deployment.keys =
      (optionalAttrs (params.typeIsCore && cfg.productionMode)
        (let keyfile = "key${toString params.i}.sk";
         in {
           "key${toString params.i}" = builtins.trace (params.name + ": using " + keyfile) {
             keyFile = ./. + "/../keys/${keyfile}";
             user = "cardano-node";
           };}))
      // {
        "topology.yaml" = {
          user = "cardano-node";
          permissions = "0400";
        }
        //
        (if ! params.typeIsExplorer
         then { keyFile = globals.topologyYaml; }
         else { text    =
         ''
wallet:
  relays: [[${concatStringsSep ", " (map (relayIx: "{\"host\": \"cardano-node-${toString relayIx}.${(envSpecific globals.environment).dnsSuffix}\", \"port\": ${toString params.port}}")
              (range 0 (globals.nRelays - 1)))}]]
  valency: 3
  fallbacks: 2
        '';});
      };
  }
