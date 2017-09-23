{ deployerIP, IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
let org = "IOHK";
    region = "eu-central-1";
    accessKeyId = IOHKaccessKeyId;
in rec {
  hydra = { config, pkgs, resources, name, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      inherit accessKeyId;
      elasticIPv4 = resources.elasticIPs.hydra-ip;
      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 200;
      associatePublicIpAddress = true;
      securityGroups = [
        resources.ec2SecurityGroups."allow-all-ssh-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-${region}-${org}"
      ];
    };
    deployment.route53.accessKeyId = config.deployment.ec2.accessKeyId;
    deployment.route53.hostName = "${name}.aws.iohkdev.io";
  };

  hydra-build-slave-1 = hydra;
  hydra-build-slave-2 = hydra;

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.keys.tarsnap = {
      keyFile = ./../static/tarsnap-cardano-deployer.secret;
      destDir = "/var/lib/keys";
    };

    deployment.ec2 = {
      inherit accessKeyId;
      elasticIPv4 = resources.elasticIPs.cardanod-ip;
      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
      securityGroups = [
        resources.ec2SecurityGroups."allow-all-ssh-${region}-${org}"
      ];
    };
  };

  resources = {
    elasticIPs = {
      hydra-ip    = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
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
      "allow-all-ssh-${region}-${org}" = {
        inherit region accessKeyId;
        description = "SSH";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = "0.0.0.0/0";
        }];
      };
      "allow-public-www-${region}-${org}" = {
        inherit region accessKeyId;
        description = "WWW";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 443; toPort = 443;
          sourceIp = "0.0.0.0/0";
        }];
      };
    };
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      disk = mkMonitor disk_monitor;
      ntp = mkMonitor ntp_monitor;
    });
  };
}
