with (import ./../lib.nix);

let
  nodeAWSConfig = import ./../modules/cardano-node-aws.nix;
  nodes = import ./cardano-nodes-config.nix;
in (mkNodes nodes nodeAWSConfig)
