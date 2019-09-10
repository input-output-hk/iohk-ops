{ globals ? {}, domain, monitorIpOverride ? null, monitoringEnv ? "ec2", ... }:

with import ../lib.nix;
{
  require = (optionals (monitoringEnv == "ec2") [
    # TODO: also make conditional on omitDetailedSecurityGroups
    ./security-groups/allow-all.nix
    # TODO: also make conditional on ! omitDetailedSecurityGroups
    ./security-groups/allow-monitoring-collection.nix
  ]);

  defaults = { config, lib, resources, ... }: {
    _file = ./global.nix;
    imports = [ ../modules/globals.nix ];
    global = {
      inherit (globals) environment systemStart topologyYaml nodeMap nRelays relays;
    } // lib.optionalAttrs (domain != null) {
      dnsDomainname = domain;
    };
    services.monitoring-exporters.graylogHost = lib.mkIf (monitorIpOverride != null) "${monitorIpOverride}:5044";

    deployment.ec2.securityGroups = if config.global.omitDetailedSecurityGroups
      then [ resources.ec2SecurityGroups."allow-all-${config.deployment.ec2.region}-${config.global.organisation}" ]
      else [];
  };
}
