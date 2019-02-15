{ name, lib, config, ... }: with lib; with types; {
  options = {
    node = mkOption {
      description = "Node-specific parameters.";
      default = {};
      type = submodule {
        options = {
          instanceType = mkOption {
            type = str;
            description = "Instance type.";
            default = null;
          };
          region = mkOption {
            type = str;
            description = "Region.";
            default = "eu-central-1";
          };
          org = mkOption {
            type = str;
            description = "Organisation.";
            default = "IOHK";
          };
          accessKeyId = mkOption {
            type = str;
            description = "Access key ID.";
            default = null;
          };
          allocateElasticIP = mkOption {
            type = bool;
            description = "Node-specific EIP allocation override.";
            default = config.cluster.allocateElasticIP;
          };
        };
      };
    };
    cluster = mkOption {
      description = "Cluster-global parameters.";
      default = {};
      type = submodule {
        options = {
          deployerIP = mkOption {
            type = str;
            description = "Deployer machine IP.";
            default = null;
          };
          toplevelDomain = mkOption {
            type = nullOr str;
            description = "Top level domain.  'null' for no DNS record.";
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
    };
    deployment.route53 = optionalAttrs (config.cluster.toplevelDomain != null) {
      accessKeyId = accessKeyId;
      hostName    = "${name}.${config.cluster.toplevelDomain}";
    };
  };
}
