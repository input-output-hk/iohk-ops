{ accessKeyId, deployerIP, ... }:
let
  region = "eu-central-1";
  org    = "IOHK";
in
{
  resources.ec2KeyPairs."cardano-keypair-${org}-${region}" = {
    inherit accessKeyId region;
  };

  resources.ec2SecurityGroups = {
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
}
