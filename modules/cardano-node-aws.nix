{ accessKeyId, relays, topologyYaml }:

with (import ./../lib.nix);

params:
  { config, resources, pkgs, nodes, options, ... }:
    let
      cfg = config.services.cardano-node;
      cardanoNodeConfigs = filter (c: c.services.cardano-node.enable)
               (map (node: node.config) (attrValues nodes));

      sgByName = x: resources.ec2SecurityGroups.${x};
      sgNames  = if   config.services.cardano-node.enable == false
                 then [ "allow-open-${params.region}" ]
                 else params.sg-names;
      sgs      = map sgByName sgNames;
    in  {
      imports = [
        ./amazon-base.nix
      ];

      services.cardano-node = {
        initialKademliaPeers = genPeersFromConfig (
          if (cfg.enableP2P && !cfg.productionMode)
          then [(head cardanoNodeConfigs)]
          else cardanoNodeConfigs
        );
        publicIP = if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else null;
        privateIP = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
        statsdServer = "127.0.0.1:8125";
      };

      # TODO: DEVOPS-8
      #deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
      deployment.ec2.region = mkForce params.region;
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor accessKeyId params.region};
      deployment.ec2.securityGroups = mkForce sgs;
      deployment.keys = (optionalAttrs (cfg.productionMode  && params.type == "core" && !cfg.hasExplorer) {
        "key${toString params.i}" = {
          keyFile = ./. + "/../keys/key${toString params.i}.sk";
          user = "cardano-node";
        };
      }) // optionalAttrs (config.services.cardano-node.enable) {
        "topology.yaml" = {
          keyFile = topologyYaml;
          user = "cardano-node";
          permissions = "0400";
        };
      } // optionalAttrs (config.services.cardano-node.enable && params.type == "relay") {
        "kademlia.yaml" = {
          user = "cardano-node";
          permissions = "0400";
          text =
          ''
identifier: '${config.services.cardano-node.dhtKey}'
peers:
${concatStringsSep "\n" (map (r: "  - host: '${r.value.name}.cardano'\n    port: ${toString config.services.cardano-node.port}") relays)}
address:
  host: '0.0.0.0'
  port: ${toString config.services.cardano-node.port}
externalAddress:
  host: '${params.name}.cardano'
  port: ${toString config.services.cardano-node.port}
          '';
        };
      };
    }
