with (import ./../lib.nix);

testIndex: region:
  { config, resources, pkgs, nodes, ... }:
    let
      cfg = config.services.cardano-node;
    in  {
      imports = [
        ./amazon-base.nix
      ];

      services.cardano-node.initialPeers = genPeersFromConfig (
        if cfg.enableP2P
        then [nodes.node0.config]
        else (lib.mapAttrsToList (name: node: node.config)
                             (lib.filter (node: node.config.services.cardano-node.enable) nodes))
      );

      deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
      deployment.ec2.region = region;
      deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor region};
      deployment.keys = optionalAttrs cfg.productionMode {
        "key${toString (testIndex + 1)}" = {
          keyFile = ./. + "/../keys/key${toString (testIndex + 1)}.sk";
          user = "cardano-node";
        };
      };
    }
