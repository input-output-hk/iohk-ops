{ IOHKaccessKeyId, environment, ... }:

with (import ./../lib.nix);

{
  explorer = { config, ... }: {
    imports = [ (import ./../modules/cardano-node-prod.nix { inherit environment; }) ];

    deployment.route53.hostName = mkForce "cardano-explorer.${(envSpecific environment).dnsSuffix}";
  };

  resources.elasticIPs.explorer-ip = { inherit region IOHKaccessKeyId; };
}
