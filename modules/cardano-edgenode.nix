{ accessKeyId, relays, topologyYaml }:

with (import ./../lib.nix);

params:
let
  instancesPerNode = 10;
  mkNode = index: {
    name = "instance${index}";
    value = {
      autoStart = true;
      privateNetwork = false;
      config = { ... }: {
        services.cardano-node = {
          #topologyFile = "todo";
          enable = true;
        };
      };
    };
  };
in { config, resources, pkgs, nodes, options, ... }:
{
  imports = [ ./amazon-base.nix ];
  deployment.ec2.region = mkForce params.region;
  deployment.ec2.accessKeyId = accessKeyId;
  deployment.ec2.keyPair = resources.ec2KeyPairs.${keypairFor accessKeyId params.region};
  containers = lib.listToAttrs (map mkNode (lib.range 1 instancesPerNode));
}
