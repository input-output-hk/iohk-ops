{ accessKeyId, nodes ? 1 , walletsPerNode ? 1
, region ? "eu-central-1"
, topologyFile, systemStart
, configKey ? "bench" }:

with import ../lib.nix;
let
  mkNode = index: {
    name = "edgenode-${toString index}";
    value = {
      imports = [ ../modules/cardano-benchmark.nix ];
      services.cardano-benchmark = {
        inherit configKey index;
      };
    };
  };
in {
  defaults = { resources, ... }: {
    services.cardano-benchmark = {
      inherit systemStart walletsPerNode;
      topologyFile = "${topologyFile}";
    };
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        instanceType = mkDefault "t2.large";
        keyPair = resources.ec2KeyPairs.edgekey;
        ebsInitialRootDiskSize = 100;
      };
    };
  };
  resources.ec2KeyPairs.edgekey = {
    inherit region accessKeyId;
  };
} // listToAttrs (map mkNode (range 1 nodes))
