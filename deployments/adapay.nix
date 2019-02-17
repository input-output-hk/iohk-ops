{ environment ? "staging", ... }:

{
  require = [ (./adapay-aws- + "${environment}.nix") ];
  network.description = "Adapay";

  defaults = { ... }: {
    imports = [
      ../modules/common.nix
      ../modules/datadog.nix
      ../modules/papertrail.nix
      ../modules/cardano-importer.nix
      ../modules/adapay.nix
      ../modules/icarus-backend.nix
    ];
    services.dd-agent.tags = [ "env:${environment}" "role:adapay" ];
    users.users = if builtins.pathExists ../static/extra-users.nix then import ../static/extra-users.nix else { };
  };
  nginx = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    services = {
      nginx = {
        enable = true;
        virtualHosts = {
          "${environment}.adapay.iohk.io" = {
            enableACME = true;
            forceSSL = true;
            locations."/".extraConfig = ''
              proxy_pass http://adapay:8081;
              proxy_set_header Host $http_host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
          };
        };
      };
    };
  };
  importer = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 8200 ];
    environment.systemPackages = with pkgs; [
      postgresql
    ];
    services = {
      cardano-importer = let
        pgConfig = import (../static/importer-pgconfig- + "${environment}.nix");
      in {
        inherit environment;
        inherit (pgConfig) pguser pgdb pghost;
        enable = true;
        pgpwFile = "/run/keys/importer-pg-pw";
      };
    };
    deployment.keys = {
      importer-pg-pw = {
        keyFile = ../static/cardano-importer-pg-pw.secret;
        user = "cardano";
      };

    };
  };
  adapay = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 8081 ];
    environment.systemPackages = with pkgs; [
      postgresql
    ];
    services = {
      icarus-backend = {
        inherit environment;
        enable = true;
      };
      adapay = {
        inherit environment;
        enable = true;
      };
    };
    deployment.keys = {
      "icarus-backend-${environment}.js" = {
        keyFile = ../static/icarus-backend + "-${environment}.js";
        user = "icarus-backend";
      };
      "adapay-${environment}.js" = {
        keyFile = ../static/adapay + "-${environment}.js";
        user = "adapay";
      };
    };
  };
}
