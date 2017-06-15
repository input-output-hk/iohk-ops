with (import ./../lib.nix);

testIndex: region:
  { config, resources, pkgs, nodes, ... }:
    let
      cfg = config.services.cardano-node;
      cardanoNodeConfigs = filter (c: c.services.cardano-node.enable)
               (map (node: node.config) (attrValues nodes));
    in  {
      imports = [
        ./amazon-base.nix
      ];

      services.cardano-node.initialPeers = genPeersFromConfig (
        if (cfg.enableP2P && !cfg.productionMode)
        then [head cardanoNodeConfigs]
        else cardanoNodeConfigs
      );

      # TODO: DEVOPS-8
      #deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
      deployment.ec2.region = region;
      deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor region};
      deployment.keys = optionalAttrs cfg.productionMode {
        "key${toString (testIndex + 1)}" = {
          keyFile = ./. + "/../keys/key${toString (testIndex + 1)}.sk";
          user = "cardano-node";
        };
      };
    }
