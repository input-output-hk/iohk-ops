{ accessKeyId }:
with (import ./../lib.nix);

let
  nodeAWSConfig = (import ./../modules/cardano-node-aws.nix) { inherit accessKeyId; };
in {
  sl-explorer = nodeAWSConfig 40 "eu-central-1";
}
