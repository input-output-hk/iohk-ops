{ name, config, resources, ... }:

with import ./../lib.nix;
{
  options = {
    global = mkOption {
      description = "IOHK global option group.";
      default = {};
      type = with types; submodule {
        options = {
          allocateElasticIP = mkOption {
            type = bool;
            description = "Whether to allocate an Elastic IP to the node.";
            default = false;
          };
          centralRegion = mkOption {
            type = str;
            description = "Region for deployer, explorer, report-server and other global services.";
            default = "eu-central-1";
          };
          defaultOrg = mkOption {
            type = enum [ "CF" "IOHK" "SGG" ];
            description = "Organisation hosting deployer, report-server and other global services.";
            default = "IOHK";
          };
          organisation = mkOption {
            type = enum [ "CF" "IOHK" "SGG" ];
            description = "Organisation managing this machine.";
            default = "IOHK";
          };
          deployerIP = mkOption {
            type = str;
            description = "The IP address of the deployer.";
            default = null;
          };
          dnsHostname = mkOption {
            type = nullOr str;
            description = "The hostname part of FQDN to advertise via DNS.";
            default = null;
          };
          dnsDomainname = mkOption {
            type = nullOr str;
            description = "The domain part of FQDN to advertise via DNS.";
            default = null;
          };
          enableEkgWeb = mkOption {
            type = bool;
            description = "Whether to start/expose EKG web frontend.";
            default = false;
          };
          nRelays = mkOption {
            type = int;
            description = "COMPUTED FROM TOPOLOGY: total N of relays.";
            default = null;
          };
          topologyYaml = mkOption {
            type = string;
            description = "DEPL-ARG PASSTHROUGH: topology file.";
            default = null;
          };
        };
      };
    };

    # To avoid creation of a new file just for a single option.
    services.report-server.logsdir = mkOption { type = types.path; default = "/var/lib/report-server"; };
  };
  config = {
    deployment = {
      ec2 = {
        elasticIPv4 = if config.global.allocateElasticIP
                      then resources.elasticIPs.${name + "-ip"} else "";
        securityGroups = map (resolveSGName resources) [
          "allow-deployer-ssh-${config.global.centralRegion}-${config.global.defaultOrg}"
        ];
      };

      route53 = optionalAttrs (config.global.dnsHostname != null) {
        accessKeyId = config.deployment.ec2.accessKeyId;
        hostName    = config.global.dnsHostname +"."+ config.global.dnsDomainname;
      };
    };
  };
}
