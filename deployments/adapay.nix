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
    services = {
      cardano-importer = {
        enable = true;
        pguser = "cardano_importer";
        pgdb = "cardano_importer";
        pghost = "adapay-staging.c9kpysxcz4mb.eu-central-1.rds.amazonaws.com";
        pgpwFile = "/run/keys/importer-pg-pw";
      };
    };
    deployment.keys = {
      importer-pg-pw.keyFile = ../static/cardano-importer-pg-pw;
    };
  };
  adapay = { config, pkgs, resources, ... }: {
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
      icarus-backend.keyFile = ../static/icarus-backend-production.js;
      adapay.keyFile = ../static/adapay-production.js;
    };
  };
}
