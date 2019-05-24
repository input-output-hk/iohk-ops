{ config, ... }:

{
  ## Universal resource logic:
  resources.ec2KeyPairs."cardano-keypair-${config.node.org}-${config.node.region}" = {
    inherit (config.node) region accessKeyId;
  };

  resources.ec2SecurityGroups = {
    "allow-deployer-ssh-${config.node.region}-${config.node.org}" = {
      _file = ./configurator.nix;
      inherit (config.node) region accessKeyId;
      description = "SSH";
      rules = [{
        protocol = "tcp"; # TCP
        fromPort = 22; toPort = 22;
        sourceIp = config.cluster.deployerIP + "/32";
      }];
    };
  };
}
