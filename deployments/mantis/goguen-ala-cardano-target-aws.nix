with builtins;
with import ../../lib.nix;
{ accessKeyId, deployerIP, ... }:
let
    org             = "IOHK";
    region          = "eu-central-1";

    sgByName        = resources: org: region: name:
      resources.ec2SecurityGroups."${name}-${region}-${org}";

    mkMachine = sgs: { resources, ... }: {
      imports = [ <module/amazon-base.nix> ];
      deployment.ec2 = {
        securityGroups = map (sgByName resources org region) (["allow-deployer-ssh"] ++ sgs);
        blockDeviceMapping = {
          "/dev/xvdf" = { size = 120; }; # resources.ebsVolumes."${hostname}-ebs"
        };
        associatePublicIpAddress = true;
      };
    };
    mkMantis   = mkMachine ["allow-mantis-public"];
    mkExplorer = mkMachine ["allow-explorer-public"];
in {
  mantis-a-0 = mkMantis; 
  mantis-a-1 = mkMantis; 
  mantis-b-0 = mkMantis; 
  mantis-b-1 = mkMantis; 
  mantis-c-0 = mkMantis;
  explorer-a = mkExplorer;

  resources = {
    elasticIPs = {
      mantis-a-0-ip = { inherit region accessKeyId; };
      mantis-a-1-ip = { inherit region accessKeyId; };
      mantis-b-0-ip = { inherit region accessKeyId; };
      mantis-b-1-ip = { inherit region accessKeyId; };
      mantis-c-0-ip = { inherit region accessKeyId; };
      explorer-a-ip = { inherit region accessKeyId; };
    };
    ec2SecurityGroups =
    let public = type: port: { protocol = type; fromPort = port ; toPort = port ; sourceIp = "0.0.0.0/0"; };
    in {
      "allow-mantis-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Mantis public ports";
        rules =  map (public "tcp") [ 5555 5679 8546 9076 30303 ]
              ++ map (public "udp") [ 8125 ];
      };
      "allow-explorer-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Goguen Explorer public ports";
        rules =  map (public "tcp") [ 80 8080 5601 ]
              ++ map (public "udp") [];
      };
    };
  };
}
