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

      deployment.ec2.region = region;
      deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor region};
      #} // lib.optionalAttrs cfg.productionMode  {
        #deployment.keys."key${toString (testIndex + 1)}" = {
        #  text = builtins.readFile (builtins.getEnv("PWD") + "/keys/key${toString (testIndex + 1)}.sk");
        #  user = "cardano-node";
        #};
    }
