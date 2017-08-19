{ accessKeyId, ... }:

with (import ./../lib.nix);

{
  explorer = { config, ... }: {
    imports = [ ./../modules/cardano-node-staging.nix ];

    deployment.route53 = {
      hostName = mkForce "cardano-explorer.aws.iohkdev.io";
    };
  };

  resources = {
    elasticIPs = {
      explorer-ip = { inherit region accessKeyId; };
    };
  };
}
