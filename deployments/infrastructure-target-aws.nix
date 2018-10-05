{ deployerIP, IOHKaccessKeyId, ... }:

with (import ../lib.nix);
let org = "IOHK";
    region = "eu-central-1";
    accessKeyId = IOHKaccessKeyId;
    mkHydra = itype: ddtags: { config, pkgs, resources, ... }: {
      imports = [
        ../modules/amazon-base.nix
      ];

      services.dd-agent.tags = ["group:linux"] ++ ddtags;

      deployment.ec2 = {
        inherit accessKeyId;
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
    };
in rec {
  hydra               = mkHydra "r3.2xlarge" ["role:hydra"];
  mantis-hydra        = mkHydra "r3.2xlarge" ["role:hydra"];
  faster-hydra        = mkHydra "c5.4xlarge" ["role:hydra"];

  buildkite-agent-1   = mkHydra "r3.2xlarge" [];
  buildkite-agent-2   = buildkite-agent-1;
  buildkite-agent-3   = buildkite-agent-1;
  buildkite-agent-4   = buildkite-agent-1;

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
      securityGroups = [
        resources.ec2SecurityGroups."allow-all-ssh-${region}-${org}"
      ];
    };
  };

  bors-ng = { config, pkgs, resources, ... }: {
    imports = [ ../modules/amazon-base.nix ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "t2.micro";
      ebsInitialRootDiskSize = mkForce 30;
      associatePublicIpAddress = true;
      securityGroups = [
        resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-https-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-http-${region}-${org}"
      ];
    };
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
    };
    datadogMonitors = (with (import ../modules/datadog-monitors.nix); {
      disk       = mkMonitor (disk_monitor "!group:hydra-and-slaves" "0.8"  "0.9");
      disk_hydra = mkMonitor (disk_monitor  "group:hydra-and-slaves" "0.95" "0.951");
      ntp  = mkMonitor ntp_monitor;
    });
  };
}
