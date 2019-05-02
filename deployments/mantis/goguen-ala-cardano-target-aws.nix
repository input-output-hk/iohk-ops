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
    mkExplorer = mkMachine ["allow-explorer-public"];
    mkFaucet   = mkMachine ["allow-faucet-public"];
    mkMantis   = mkMachine ["allow-mantis-public"];
in {
  explorer-a = mkExplorer;

  faucet-a   = mkFaucet;

  mantis-a-0 = mkMantis;
  mantis-a-1 = mkMantis;
  mantis-b-0 = mkMantis;
  mantis-b-1 = mkMantis;
  mantis-c-0 = mkMantis;

  resources = {
    elasticIPs = {
      explorer-a-ip = { inherit region accessKeyId; };

      faucet-a-ip   = { inherit region accessKeyId; };

      mantis-a-0-ip = { inherit region accessKeyId; };
      mantis-a-1-ip = { inherit region accessKeyId; };
      mantis-b-0-ip = { inherit region accessKeyId; };
      mantis-b-1-ip = { inherit region accessKeyId; };
      mantis-c-0-ip = { inherit region accessKeyId; };
    };

    ec2SecurityGroups =
    let public = protocol: port: { inherit protocol; fromPort = port ; toPort = port ; sourceIp = "0.0.0.0/0"; };
    in {
      "allow-explorer-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Goguen Explorer public ports";
        rules =  map (public "tcp") [ 80 8080 5601 ]
              ++ map (public "udp") [];
      };
      "allow-faucet-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Goguen Faucet public ports";
        rules =  map (public "tcp") [ 8099 5555 ]
              ++ map (public "udp") [ 8125 ];
      };
      "allow-mantis-public-${region}-${org}" = {
        inherit region accessKeyId;
        description = "Mantis public ports";
        rules =  map (public "tcp") [ 5555 5679 8546 9076 30303 ]
              ++ map (public "udp") [ 8125 ];
      };
    };
  };
}
