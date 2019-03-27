# Since we aim to be able to use the options defined in `parameters.nix` both in
# nixops and NixOS contexts we can't use anything node-specific there.
# `name` is node-specific so everything that uses it directly or indirectly had
# to be taken out of `parameters.nix`. That way we can use `evalModules` it
# and avoid having to repeat all parameters in all `configs/*.nix` files.
{ config, lib, name, resources, ... }:

{
  node.fqdn = with config.cluster; lib.mkDefault "${name}.${config.cluster.name}.${toplevelDomain}";

  deployment.ec2 = {
    inherit (config.node) accessKeyId region instanceType;
  } // lib.optionalAttrs config.node.allocateElasticIP {
    elasticIPv4 = resources.elasticIPs."${name}-ip";
  };
  deployment.route53 = lib.mkIf (config.cluster.hostedZone != null) {
    inherit (config.node) accessKeyId;
    hostName    = "${config.node.fqdn}";
  };
}
