{ IOHKaccessKeyId, IOHKroute53accessKeyId, ... }:

with (import ../lib.nix);
let region = "eu-central-1";
    accessKeyId = IOHKaccessKeyId;
    route53accessKeyId = IOHKroute53accessKeyId;
in {
  network.description = "IOHK infrastructure production";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ../modules/papertrail.nix
      ../modules/datadog.nix
    ];

    services.dd-agent.tags = ["env:production" "depl:${config.deployment.name}"];

    deployment.ec2.elasticIPv4 = resources.elasticIPs.hydra-ip;

    deployment.route53.accessKeyId = route53accessKeyId;
    deployment.route53.hostName = "hydra.aws.iohkdev.io";
  };

  mantis-hydra = { config, pkgs, resources, ... }: {
    # Differences from the main Hydra:
    # 1. builds are not sandboxed
    # 2. hydra builds things without slaves
    imports = [
      ../modules/papertrail.nix
      ../modules/datadog.nix
    ];

    services.dd-agent.tags = ["env:production" "depl:${config.deployment.name}"];

    deployment.ec2.elasticIPv4 = resources.elasticIPs.mantis-hydra-ip;

    deployment.route53.accessKeyId = route53accessKeyId;
    deployment.route53.hostName = "mantis-hydra.aws.iohkdev.io";
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/datadog.nix
      ../modules/papertrail.nix
      ../modules/deployer.nix
    ];

    services.dd-agent.tags = ["env:production" "depl:${config.deployment.name}"];

    deployment.keys.tarsnap = {
      keyFile = ../static/tarsnap-cardano-deployer.secret;
      destDir = "/var/lib/keys";
    };

    users = {
      users.live-production = {
        description     = "cardano live-production";
        group           = "live-production";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devOpsKeys;
      };
      groups.live-production = {};
      users.staging = {
        description     = "cardano staging";
        group           = "staging";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.staging = {};
    };

    services.tarsnap = {
      enable = true;
      keyfile = "/var/lib/keys/tarsnap";
      archives.cardano-deployer = {
        directories = [
          "/home/live-production/.ec2-keys"
          "/home/live-production/.aws"
          "/home/live-production/.nixops"
          "/etc/"
        ];
      };
    };

    deployment.ec2.elasticIPv4 = resources.elasticIPs.cardanod-ip;
  };

  bors-ng = { config, pkgs, resources, ... }: let
    hostName = "bors-ng.aws.iohkdev.io";
    keysDir = "/var/lib/keys";
  in {
    imports = [
      ../modules/datadog.nix
      ../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:production" "depl:${config.deployment.name}"];

    deployment.ec2.elasticIPv4 = resources.elasticIPs.bors-ng-ip;
    deployment.route53.accessKeyId = route53accessKeyId;
    deployment.route53.hostName = hostName;

    services.bors-ng = {
      publicHost = hostName;
      secretKeyBaseFile = "${keysDir}/bors-ng-secret-key-base";
      github = {
        clientID = "Iv1.17382ed95b58d1a8";
        clientSecretFile = "${keysDir}/bors-ng-github-client-secret";
        integrationID = 17473;
        integrationPEMFile = "${keysDir}/bors-ng-github-integration.pem";
        webhookSecretFile = "${keysDir}/bors-ng-github-webhook-secret";
      };
    };
    systemd.services.bors-ng.after = [ "keys.target" ];
    systemd.services.bors-ng.requires = [ "keys.target" ];

    deployment.keys.bors-ng-secret-key-base = {
      keyFile = ../static/bors-ng-secret-key-base;
      destDir = "/var/lib/keys";
    };
    deployment.keys.bors-ng-github-client-secret = {
      keyFile = ../static/bors-ng-github-client-secret;
      destDir = "/var/lib/keys";
    };
    deployment.keys.bors-ng-github-integration-pem = {
      keyFile = ../static/bors-ng-github-integration.pem;
      destDir = "/var/lib/keys";
    };
    deployment.keys.bors-ng-github-webhook-secret = {
      keyFile = ../static/bors-ng-github-webhook-secret;
      destDir = "/var/lib/keys";
    };
  };
}
