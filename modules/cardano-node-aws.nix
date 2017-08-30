{ accessKeyId, topologyYaml, environment }:

with (import ./../lib.nix);

params:
  { config, resources, pkgs, nodes, options, ... }:
    let
      cfg = config.services.cardano-node;
      sgByName = x: resources.ec2SecurityGroups.${x};
    in  {
      imports = [
        ./amazon-base.nix
      ];

      services.cardano-node = {
        publicIP = if options.networking.publicIPv4.isDefined then config.networking.publicIPv4 else null;
        privateIP = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
        statsdServer = "127.0.0.1:8125";
      };

      # TODO: DEVOPS-8
      #deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
      deployment.ec2.region = mkForce params.region;
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor accessKeyId params.region};
      deployment.ec2.securityGroups = mkForce (map sgByName params.sgNames);
      deployment.keys = (optionalAttrs (cfg.productionMode  && params.type == "core")
      (let keyfile = "key${toString params.i}.sk";
       in {
         "key${toString params.i}" = builtins.trace (params.name + ": using " + keyfile) {
           keyFile = ./. + "/../keys/${keyfile}";
           user = "cardano-node";
         };
       })) // optionalAttrs (config.services.cardano-node.enable) {
        "topology.yaml" =
        {
          user = "cardano-node";
          permissions = "0400";
        } //
        (if config.services.cardano-node.nodeName != "explorer"
        then { keyFile = topologyYaml; }
        else { text    =
        ''
wallet:
  relays: [[${concatStringsSep ", " (map (relayIx: "{\"host\": \"cardano-node-${toString relayIx}.${(envSpecific environment).dnsSuffix}\", \"port\": 3000}")
              (range 0 (params.nRelays - 1)))}]]
  valency: 3
  fallbacks: 2
        '';});
        };
      }
