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
    elasticIPs = {
      hydra-ip = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
    };
    datadogMonitors = (with (import ./../modules/datadog-monitors.nix); {
      disk = mkMonitor disk_monitor;
      ntp = mkMonitor ntp_monitor;
    });
  };
}
