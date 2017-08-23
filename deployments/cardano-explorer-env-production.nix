{ accessKeyId, environment, ... }:

with (import ./../lib.nix);

{
  explorer = { config, ... }: {
    imports = [ ./../modules/cardano-node-prod.nix ];

    deployment.route53 = {
      hostName = mkForce "cardano-explorer.${(envSpecific environment).dnsSuffix}";
    };
  };

  resources = {
    elasticIPs = {
      explorer-ip = { inherit region accessKeyId; };
    };
  };
}
