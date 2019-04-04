{ ... }:

with (import ./../lib.nix);
{
  network.description = "IOHK infrastructure staging";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/papertrail.nix
    ];

    ##
    security.acme.certs = mkForce {};
    services.nginx.enable = mkForce false;

  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/papertrail.nix
    ];

    deployment.keys.tarsnap = {
      keyFile = ./../static/tarsnap-cardano-deployer.secret;
      destDir = "/var/lib/keys";
    };

    users = {
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
          "/home/staging/.ec2-keys"
          "/home/staging/.aws"
          "/home/staging/.nixops"
          "/etc/"
        ];
      };
    };

  };

  bors-ng = { config, pkgs, resources, ... }: let
    hostName = "bors-ng.awstest2.iohkdev.io";
  in {
    imports = [
      ../modules/papertrail.nix
    ];

    services.bors-ng = let
      placeholder = pkgs.writeText "placeholder" "staging";
    in {
      publicHost = hostName;
      secretKeyBaseFile = placeholder;
      github = {
        clientID = "staging";
        clientSecretFile = placeholder;
        integrationID = 0;
        integrationPEMFile = placeholder;
        webhookSecretFile = placeholder;
      };
    };
  };
}
