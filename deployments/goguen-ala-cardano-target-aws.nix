with builtins;
with import ./../lib.nix;
{ accessKeyId, deployerIP, ... }:
let
    org             = "IOHK";
    region          = "eu-central-1";
    mkMantisMachine = hostname: { nodes, resources, pkgs, config, ... }:
rec {
      imports = [
        ../modules/amazon-base.nix
      ];
      deployment.ec2 = {
        inherit accessKeyId;
        associatePublicIpAddress = true;
        elasticIPv4 = resources.elasticIPs."${hostname}-ip";
        securityGroups = [
          resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
        ];
      };
      deployment.route53.accessKeyId = accessKeyId;
      deployment.route53.hostName = "${hostname}.${config.deployment.name}.aws.iohkdev.io";
};
in {
  mantis-a-0 = mkMantisMachine "mantis-a-0"; 
  mantis-a-1 = mkMantisMachine "mantis-a-1"; 
  mantis-b-0 = mkMantisMachine "mantis-b-0"; 
  mantis-b-1 = mkMantisMachine "mantis-b-1"; 
  mantis-c-0 = mkMantisMachine "mantis-c-0";
  resources = {
    elasticIPs = {
      mantis-a-0-ip = { inherit region accessKeyId; };
      mantis-a-1-ip = { inherit region accessKeyId; };
      mantis-b-0-ip = { inherit region accessKeyId; };
      mantis-b-1-ip = { inherit region accessKeyId; };
      mantis-c-0-ip = { inherit region accessKeyId; };
    };
    ec2KeyPairs = {
      cardano-keypair-IOHK-eu-central-1 = {
        inherit region accessKeyId;
        description = "Keypair for ${org}/${region}";
      };
    };
    ec2SecurityGroups = {
      "allow-deployer-ssh-${region}-${org}" = {
        inherit region accessKeyId;
        description = "SSH";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = deployerIP + "/32";
        }];
      };
    };
  };
}
