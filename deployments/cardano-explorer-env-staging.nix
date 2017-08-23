{ accessKeyId, environment, ... }:

with (import ./../lib.nix);

{
  explorer = { config, ... }: {
    imports = [ (import ./../modules/cardano-node-staging.nix { inherit environment; }) ];

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
