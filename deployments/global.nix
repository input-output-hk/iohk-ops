{ globals, domain, ... }:

{
  require = [
    # TODO, only import if config.global.omitDetailedSecurityGroups
    ./security-groups/allow-all.nix
  ];
  defaults = { config, lib, resources, ... }: {
    _file = ./global.nix;
    imports = [ ../modules/globals.nix ];
    global = {
      inherit (globals) environment systemStart topologyYaml nodeMap nRelays relays;
    } // lib.optionalAttrs (domain != null) {
      dnsDomainname = domain;
    };

    deployment.ec2.securityGroups = if config.global.omitDetailedSecurityGroups
      then [ resources.ec2SecurityGroups."allow-all-${config.deployment.ec2.region}-${config.global.organisation}" ]
      else [];
  };
}
