{ deployerIP, IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
let org = "IOHK";
    region = "eu-central-1";
    accessKeyId = IOHKaccessKeyId;
in rec {
  hydra = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      inherit accessKeyId;
      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 200;
      associatePublicIpAddress = true;
      securityGroups = [
        resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
        resources.ec2SecurityGroups."allow-public-www-${region}-${org}"
      ];
    };
  };

  hydra-build-slave-1 = hydra;
  hydra-build-slave-2 = hydra;

  buildkite-agent-1   = hydra;
  buildkite-agent-2   = hydra;
  buildkite-agent-3   = hydra;
  buildkite-agent-4   = hydra;

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
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
    elasticIPs = {
      hydra-ip    = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
    };
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      disk       = mkMonitor (disk_monitor "!group:hydra-and-slaves" "0.8"  "0.9");
      disk_hydra = mkMonitor (disk_monitor  "group:hydra-and-slaves" "0.95" "0.951");
      ntp  = mkMonitor ntp_monitor;
    });
  };
}
