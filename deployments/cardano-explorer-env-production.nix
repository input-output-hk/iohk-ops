{ accessKeyId, ... }:

with (import ./../lib.nix);

{
  sl-explorer = { config, ... }: {
    imports = [ ./../modules/cardano-node-prod.nix ];

    deployment.route53 = {
      hostName = mkForce "cardano-explorer.aws.iohk.io";
    };
  };

  resources = {
    elasticIPs = {
      nodeip40 = { inherit region accessKeyId; };
    };
  };
}
