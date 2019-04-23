{ ... }:

with (import ../lib.nix);
let
    mkHydra = hostname: { config, pkgs, resources, ... }: {

      imports = [
        ../modules/papertrail.nix
      ];
    };
in {
  network.description = "IOHK infrastructure production";

  hydra        = mkHydra "hydra";
  faster-hydra = mkHydra "faster-hydra";
  mantis-hydra = mkHydra "mantis-hydra";

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/papertrail.nix
      ../modules/deployer.nix
    ];

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
          "/home/live-production/.aws"
          "/home/live-production/.nixops"
          "/etc/"
        ];
      };
    };

  };

  bors-ng = { config, pkgs, resources, ... }: let
    hostName = "bors-ng.aws.iohkdev.io";
    keysDir = "/var/lib/keys";
  in {
    imports = [
      ../modules/papertrail.nix
    ];

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
    users.users.bors-ng.extraGroups = [ "keys" ];

    deployment.keys.bors-ng-secret-key-base = {
      keyFile = ../static/bors-ng-secret-key-base;
      destDir = "/var/lib/keys";
      user = "bors-ng";
    };
    deployment.keys.bors-ng-github-client-secret = {
      keyFile = ../static/bors-ng-github-client-secret;
      destDir = "/var/lib/keys";
      user = "bors-ng";
    };
    deployment.keys."bors-ng-github-integration.pem" = {
      keyFile = ../static/bors-ng-github-integration.pem;
      destDir = "/var/lib/keys";
      user = "bors-ng";
    };
    deployment.keys.bors-ng-github-webhook-secret = {
      keyFile = ../static/bors-ng-github-webhook-secret;
      destDir = "/var/lib/keys";
      user = "bors-ng";
    };
  };
  log-classifier = { config, pkgs, resources, ... }: let
    hostName = "log-classifier.aws.iohkdev.io";
    keysDir = "/var/lib/keys";
  in {
    imports = [
      ../modules/papertrail.nix
      ../modules/log-classifier.nix
      ../modules/common.nix
    ];

    services.log-classifier.domain = "log-classifier.aws.iohkdev.io";
  };
}
