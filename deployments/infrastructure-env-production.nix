{ IOHKaccessKeyId, deployerIP, ... }:

let region = "eu-central-1";
    org    = "IOHK";
in

with (import ./../lib.nix);
{
  network.description = "IOHK infrastructure production";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/papertrail.nix
      ./../modules/datadog.nix
    ];

    services.dd-agent.tags = ["env:production"];

    deployment.ec2 = {
      elasticIPv4 = resources.elasticIPs.hydra-ip;
    };
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/common.nix
      ./../modules/amazon-base.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:production"];

    deployment.ec2 = {
      elasticIPv4 = resources.elasticIPs.cardanod-ip;
    };
  };

  resources = {
    ec2SecurityGroups = {
      "allow-deployer-ssh-${region}-${org}" = {
        inherit region;
        accessKeyId = IOHKaccessKeyId;
        description = "SSH";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = deployerIP + "/32";
        }];
      };
      "allow-public-www-${region}-${org}" = {
        inherit region;
        accessKeyId = IOHKaccessKeyId;
        description = "WWW";
        rules = [{
          protocol = "tcp"; # TCP
          fromPort = 443; toPort = 443;
          sourceIp = "0.0.0.0/0";
        }];
      };
    };
    elasticIPs = {
      hydra-ip = { region = centralRegion; accessKeyId = IOHKaccessKeyId; };
      cardanod-ip = { region = centralRegion; accessKeyId = IOHKaccessKeyId; };
    };
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      disk = mkMonitor disk_monitor;
      ntp = mkMonitor ntp_monitor;
    });
  };
}
