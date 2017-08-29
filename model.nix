{ ... }:

let
  region     = "eu-central-1";
  akId-a     = "iohk";
  akId-b     = "third-party";
  deployerIP = "35.156.156.28";
  mkNode     = accessKeyId: kpName: eipName: sgName:
  { config, resources, pkgs, nodes, options, ... }: {
     deployment.targetEnv        = "ec2";
     deployment.ec2.accessKeyId  = accessKeyId;
     deployment.ec2.instanceType = "t2.large";
     deployment.ec2.region       = region;
     deployment.ec2.keyPair      = resources.ec2KeyPairs.${kpName};

     networking.hostName = "repro-host";

     deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.${sgName} ];

     deployment.ec2.elasticIPv4 = resources.elasticIPs.${eipName};
  };
  mkSG       = accessKeyId: {
        inherit accessKeyId region;
        description = "model SSH SG";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = deployerIP + "/32";
        }];
      };
in
{
  network.description = "Cardano Federated";

  node-a = mkNode akId-a "keypair-a" "node-a-ip" "sg-ssh-a";
  node-b = mkNode akId-b "keypair-b" "node-b-ip" "sg-ssh-b";

  resources = {
    ec2KeyPairs = {
      keypair-a = { name = "keypair-a"; accessKeyId = akId-a; inherit region; };
      keypair-b = { name = "keypair-b"; accessKeyId = akId-b; inherit region; };
    };
    elasticIPs = { 
      node-a-ip = { name = "node-a-ip"; accessKeyId = akId-a; inherit region; };
      node-b-ip = { name = "node-a-ip"; accessKeyId = akId-b; inherit region; };
    };
    ec2SecurityGroups = {
      sg-ssh-a  = mkSG akId-a;
      sg-ssh-b  = mkSG akId-b;
    };
  };

}
