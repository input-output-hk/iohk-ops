with builtins;
with import ../../lib.nix;
{ accessKeyId, deployerIP, ... }:
let
    org             = "IOHK";
    region          = "eu-central-1";
    mkMantisMachine = hostname: { nodes, resources, pkgs, config, ... }:
rec {
      imports = [ <module/amazon-base.nix> ];
      deployment.ec2 = {
        securityGroups = [
          resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
          resources.ec2SecurityGroups."allow-mantis-public-${region}-${org}"
        ];
        blockDeviceMapping = {
          "/dev/xvdf" = { size = 120; }; # resources.ebsVolumes."${hostname}-ebs"
        };
        associatePublicIpAddress = true;
      };
      # deployment.route53.hostName = "${hostname}.${config.deployment.name}.dev-mantis.iohkdev.io";
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
    # ebsVolumes = let size = 120; in {
    #   mantis-a-0-ebs = { inherit region accessKeyId size; };
    #   mantis-a-1-ebs = { inherit region accessKeyId size; };
    #   mantis-b-0-ebs = { inherit region accessKeyId size; };
    #   mantis-b-1-ebs = { inherit region accessKeyId size; };
    #   mantis-c-0-ebs = { inherit region accessKeyId size; };
    # };
    ec2SecurityGroups = {
      "allow-mantis-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Mantis public ports";
        rules = [
          { protocol = "tcp"; fromPort = 5555 ; toPort = 5555 ; sourceIp = "0.0.0.0/0"; }
          { protocol = "tcp"; fromPort = 5679 ; toPort = 5679 ; sourceIp = "0.0.0.0/0"; }
          { protocol = "udp"; fromPort = 8125 ; toPort = 8125 ; sourceIp = "0.0.0.0/0"; }
          { protocol = "tcp"; fromPort = 8546 ; toPort = 8546 ; sourceIp = "0.0.0.0/0"; }
          { protocol = "tcp"; fromPort = 9076 ; toPort = 9076 ; sourceIp = "0.0.0.0/0"; }
          { protocol = "tcp"; fromPort = 30303; toPort = 30303; sourceIp = "0.0.0.0/0"; }
        ];
      };
    };
    # route53HostedZones.hz = { config, ... }: {
    #   name = "${config.deployment.name}.dev-mantis.iohkdev.io.";
    #   inherit region accessKeyId;
    # };
  };
}
