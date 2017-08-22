{ accessKeyId, region }:

with (import ./../lib.nix);

let
  instancesPerNode = 10;
  config = import ../config.nix;
  mkNode = index: {
    name = "instance${toString index}";
    value = {
      autoStart = true;
      privateNetwork = false;
      config = { ... }: {
        imports = [ ./common.nix ];
        services.cardano-node = {
          #topologyFile = "todo";
          enable = true;
          nodeIndex = 50;
          inherit (config) genesisN enableP2P;
          nodeName = "edgenode";
          type = "relay"; # TODO, pick a better type
        };
      };
    };
  };
in { config, resources, pkgs, nodes, options, ... }:
{
  imports = [ ./amazon-base.nix ];
  deployment.ec2.region = mkForce region;
  deployment.ec2.accessKeyId = accessKeyId;
  deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor accessKeyId region};
  containers = listToAttrs (map mkNode (range 1 instancesPerNode));
}
