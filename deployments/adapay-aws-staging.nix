{ environment ? "staging"
, deployerIP ? ""
}:
let
  accessKeyId = "adapay";
  region = "eu-central-1";
  zone = "eu-central-1a";
in {
  requires = [ ./adapay.nix ];
  adapay = { config, pkgs, lib, resources, ... }: {
    ec2.hvm = true;

    deployment = {
      ec2 = {
        inherit accessKeyId region zone;
        ebsInitialRootDiskSize = 20;
        instanceType = "t2.medium";
        keyPair = resources.ec2KeyPairs.adapayKey;
        elasticIPv4 = resources.elasticIPs.adapayIP;
      };
      targetEnv = "ec2";
      targetHost = "localhost";
      route53 = {
        inherit accessKeyId;
        hostname = "${environment}.adapay.iohk.io";
      };
    };

    # Don't reconfigure the system from EC2 userdata on next startup
    systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
  };
  resources = {
    elasticIPs.adapayIP = {
      inherit accessKeyId region;
      vpc = true;
    };
    ec2KeyPairs.adapayKey = {
      inherit accessKeyId region;
    };
    ec2SecurityGroups.adapaySG = {
      inherit accessKeyId region;
      rules = let
        allowPortSingle = source: port: {
            fromPort = port;
            toPort = port;
            sourceIp = source;
        };
        allowPortPublic = allowPortSingle "0.0.0.0/0";
      in [
        (allowPortSingle deployerIP 22)
        ] ++
        (map allowPortPublic [ 80 443 ]);
    };
  };
}
