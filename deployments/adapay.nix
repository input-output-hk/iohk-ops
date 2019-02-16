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
  };
  nginx = { config, pkgs, resources, ... }: {
    services = {
      nginx = {
        enable = true;
      };
    };
  };
  importer = { config, pkgs, resources, ... }: {
    environment.systemPackages = with pkgs; [
      postgresql
    ];
    services = {
      cardano-importer = {
        inherit environment;
        enable = true;
        pguser = "importer_rw";
        pgdb = "importer";
        pghost = "adapay-staging.c9kpysxcz4mb.eu-central-1.rds.amazonaws.com";
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
