{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.bors-ng;

in {
  options.services.bors-ng = {
    enable = mkEnableOption "bors-ng";

    dockerImage = mkOption {
      default = "borsng/bors-ng:latest";
      description = ''
        The name of the docker image to pull before every startup.
      '';
      type = types.str;
    };

    publicHost = mkOption {
      description = ''
        The hostname that will be used to access bors-ng.
      '';
      example = "bors-app.herokuapp.com";
      type = types.str;
    };

    port = mkOption {
      description = ''
        The port for the bors-ng service to listen on.
      '';
      example = 80;
      default = 4000;
      type = types.int;
    };

    databaseURL = mkOption {
      description = ''
        The PostgreSQL URL.
      '';
      example = "postgresql:///bors_ng";
      type = types.str;
    };

    secretKeyBaseFile = mkOption {
      description = ''
        Path to a file containing the secret key base for bors-ng.
        This key must be at least 64 bytes long.
      '';
      example = "/run/keys/bors-ng-secret-key";
      type = types.path;
    };

    github = mkOption {
      description = "GitHub Integration options";
      type = types.submodule {
        options = {
          clientID = mkOption {
            description = "GitHub OAuth Client ID.";
            type = types.str;
          };
          clientSecretFile = mkOption {
            description = "Path to a file containing the GitHub OAuth client secret.";
            type = types.path;
          };
          integrationID = mkOption {
            description = "Integration ID of the app registered in GitHub.";
            type = types.int;
          };
          integrationPEMFile = mkOption {
            description = ''
              Path to a file in PEM format containing the integration secret key
              of the app registered in GitHub.
            '';
            type = types.path;
          };
          webhookSecretFile = mkOption {
            description = ''
              Path to a file containing the GitHub webhook secret.
            '';
            type = types.path;
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;

    users.users.bors-ng = {
      description = "bors-ng User";
      group = "bors-ng";
      home = "/var/lib/bors-ng";
      createHome = true;
      extraGroups = [ "docker" ];
      isSystemUser = true;
    };
    users.groups.bors-ng = { };

    systemd.services.bors-ng = let
      # Note: secrets are visible here.
      dockerArgs = lib.concatStringsSep " " [
        "--network=host"
        "-e PUBLIC_HOST=${cfg.publicHost}"
        "-e PORT=${toString cfg.port}"
        "-e SECRET_KEY_BASE=\"$(head -n1 ${cfg.secretKeyBaseFile})\""
        "-e GITHUB_CLIENT_ID=\"${cfg.github.clientID}\""
        "-e GITHUB_CLIENT_SECRET=\"$(head -n1 ${cfg.github.clientSecretFile})\""
        "-e GITHUB_INTEGRATION_ID=${toString cfg.github.integrationID}"
        "-e GITHUB_INTEGRATION_PEM=\"$(base64 -w0 ${cfg.github.integrationPEMFile})\""
        "-e GITHUB_WEBHOOK_SECRET=\"$(head -n1 ${cfg.github.webhookSecretFile})\""
        "-e DATABASE_URL=\"${cfg.databaseURL}\""
        "-e DATABASE_USE_SSL=false"
        "-e DATABASE_AUTO_MIGRATE=true"
      ];
    in {
      description = "bors-ng Merge Bot";
      path = [ pkgs.docker pkgs.coreutils ];
      wantedBy = [ "multi-user.target" ];
      after = [ "docker.service" ];
      requires = [ "docker.service" ];
      serviceConfig = {
        User = "bors-ng";
        Group = "bors-ng";
        Restart = "always";
        RestartSec = "2s";
        TimeoutStartSec = "0";
        ExecStop = "${pkgs.docker}/bin/docker stop bors-ng";
        ExecReload = "${pkgs.docker}/bin/docker restart bors-ng";
      };
      preStart = ''
        # Pull the image only if it doesn't already exist.
        # We don't want to auto-update.
        if [ -z "$(docker images --format '{{ .ID }}' ${cfg.dockerImage})" ]; then
          docker pull ${cfg.dockerImage} || true
        fi
        docker stop bors-ng || true
      '';
      script = ''
        docker run --name bors-ng --restart=unless-stopped ${dockerArgs} ${cfg.dockerImage}
      '';
    };

  };
}
