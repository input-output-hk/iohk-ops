{ name, lib, config, ... }: with lib; with types; {
  options = {
    ##
    ##
    node = mkOption {
      description = "Node-specific parameters.";
      default = {};
      type = submodule {
        options = {
          ##
          ## Mandatory configuration:
          region = mkOption {
            type = str;
            description = "Region.  Must be set (use deployments/config.nix).";
            default = "eu-central-1";
          };
          accessKeyId = mkOption {
            type = str;
            description = "Access key ID.  Must be set (use deployments/config.nix).";
            default = null;
          };
          ##
          ## Non-mandatory configuration:
          instanceType = mkOption {
            type = str;
            description = "Instance type.";
            default = "t2.large";
          };
          org = mkOption {
            type = str;
            description = "Organisation.";
            default = "IOHK";
          };
          allocateElasticIP = mkOption {
            type = bool;
            description = "Node-specific EIP allocation override.  You must provide ${name}-ip.";
            default = config.cluster.allocateElasticIP;
          };
          fqdn = mkOption {
            type = nullOr str;
            description = "Node's FQDN, which defaults to ${name}.${toplevelDomain}";
            default = if config.cluster.toplevelDomain != null
                      then name + "." + config.cluster.toplevelDomain
                      else null;
          };
        };
      };
    };
    ##
    ##
    cluster = mkOption {
      description = "Cluster-wide configuration.";
      default = {};
      type = submodule {
        options = {
          ##
          ## Mandatory configuration:
          deployerIP = mkOption {
            type = str;
            description = "Deployer machine IP.  Must be set (use deployments/config.nix).";
            default = null;
          };
          ##
          ## Non-mandatory configuration:
          toplevelDomain = mkOption {
            type = nullOr str;
            description = "Top level domain.  'null' for no DNS record.";
            default = config.cluster.hostedZone;
          };
          hostedZone = mkOption {
            type = nullOr str;
            description = "Hosted zone";
            default = null;
          };
          allocateElasticIP = mkOption {
            type = bool;
            description = "Cluster-wide EIP allocation policy.";
            default = false;
          };
        };
      };
    };
  };
  config = {
    deployment.ec2 = {
      inherit (config.node) accessKeyId region instanceType;
    } // optionalAttrs config.node.allocateElasticIP {
      elasticIPv4 = resources.elasticIPs."${name}-ip";
    };
    deployment.route53 = optionalAttrs (config.cluster.toplevelDomain != null) {
      inherit (config.node) accessKeyId;
      hostName    = "${name}.${config.cluster.toplevelDomain}";
    };
  };
}
