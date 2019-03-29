{ deployerIP, IOHKaccessKeyId, IOHKroute53accessKeyId, ... }:

with (import ../lib.nix);
let org = "IOHK";
    region = "eu-central-1";
    accessKeyId = IOHKaccessKeyId;
    route53accessKeyId = IOHKroute53accessKeyId;
    mkHydra = hostname: itype: ddtags: { config, pkgs, resources, ... }: {
      imports = [
        ../modules/amazon-base.nix
      ];

      deployment.ec2 = {
        inherit accessKeyId;
        elasticIPv4 = resources.elasticIPs."${hostname}-ip";
        instanceType = mkForce itype;
        ebsInitialRootDiskSize = mkForce 200;
        associatePublicIpAddress = true;
        securityGroups = [
          resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
          resources.ec2SecurityGroups."allow-hydra-ssh-${region}-${org}"
          resources.ec2SecurityGroups."allow-public-www-https-${region}-${org}"
          resources.ec2SecurityGroups."allow-public-www-http-${region}-${org}"
        ];
      };
      deployment.route53.accessKeyId = route53accessKeyId;
      deployment.route53.hostName = "${hostname}.aws.iohkdev.io";
    };
in rec {
  hydra               = mkHydra "hydra" "r3.2xlarge" ["role:hydra"];
  mantis-hydra        = mkHydra "mantis-hydra" "r3.2xlarge" ["role:hydra"];
  faster-hydra        = mkHydra "faster-hydra" "c5.4xlarge" ["role:hydra"];

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
      elasticIPv4 = resources.elasticIPs.cardanod-ip;
      securityGroups = [
        resources.ec2SecurityGroups."allow-all-ssh-${region}-${org}"
      ];
    };

    services.tarsnap = {
      archives.cardano-deployer = {
        directories = [
          "/home/live-production/.ec2-keys"
        ];
      };
    };
  };

  bors-ng = { config, pkgs, resources, ... }: let
    hostName = "bors-ng.aws.iohkdev.io";
  in {
    imports = [ ../modules/amazon-base.nix ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "t2.micro";
      ebsInitialRootDiskSize = mkForce 30;
      associatePublicIpAddress = true;
      elasticIPv4 = resources.elasticIPs.bors-ng-ip;
      securityGroups = [
        resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-https-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-http-${region}-${org}"
      ];
    };
    deployment.route53.accessKeyId = route53accessKeyId;
    deployment.route53.hostName = hostName;
  };

  log-classifier = { config, pkgs, resources, ... }: let
    hostName = "log-classifier.aws.iohkdev.io";
  in {
    imports = [ ../modules/amazon-base.nix ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "t2.micro";
      ebsInitialRootDiskSize = mkForce 30;
      associatePublicIpAddress = true;
      elasticIPv4 = resources.elasticIPs.log-classifier-ip;
      securityGroups = [
        resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-https-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-http-${region}-${org}"
      ];
    };
    deployment.route53.accessKeyId = route53accessKeyId;
    deployment.route53.hostName = hostName;
  };

  resources = {
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
      "allow-hydra-ssh-${region}-${org}" = { resources, ...}: {
        inherit region accessKeyId;
        description = "SSH";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = resources.elasticIPs.hydra-ip;
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
      "allow-public-www-https-${region}-${org}" = {
        inherit region accessKeyId;
        description = "WWW-https";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 443; toPort = 443;
          sourceIp = "0.0.0.0/0";
        }];
      };
      "allow-public-www-http-${region}-${org}" = {
        inherit region accessKeyId;
        description = "WWW-http";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 80; toPort = 80;
          sourceIp = "0.0.0.0/0";
        }];
      };
    };
    elasticIPs = {
      hydra-ip        = { inherit region accessKeyId; };
      faster-hydra-ip = { inherit region accessKeyId; };
      mantis-hydra-ip = { inherit region accessKeyId; };
      cardanod-ip     = { inherit region accessKeyId; };
      bors-ng-ip      = { inherit region accessKeyId; };
      log-classifier-ip      = { inherit region accessKeyId; };
    };
    datadogMonitors = (with (import ../modules/datadog-monitors.nix); {
      disk       = mkMonitor (disk_monitor "!group:hydra-and-slaves,!group:buildkite-agents" "0.8"  "0.9");
      disk_buildkite = mkMonitor (disk_monitor  "group:buildkite-agents" "0.9" "0.95");
      disk_hydra = mkMonitor (disk_monitor  "group:hydra-and-slaves" "0.95" "0.951");
      ntp  = mkMonitor ntp_monitor;
    });
  };
}
