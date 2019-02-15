{ environment ? "staging", ... }:

{
  network.description = "Adapay";

  defaults = { ... }: {
    imports = [
      #../modules/common.nix
      #../modules/datadog.nix
      #../modules/papertrail.nix
      #../modules/cardano-importer.nix
      #../modules/adapay.nix
      #../modules/icarus-backend.nix
    ];
    #services.dd-agent.tags = [ "env:${environment}" "role:adapay" ];
  };
  #nginx = { config, pkgs, resources, ... }: {
  #  services = {
  #    nginx = {
  #      enable = true;
  #    };
  #  };
  #};
  #importer = { config, pkgs, resources, ... }: {
  #  services = {
  #    #cardano-importer = {
  #    #  enable = false;
  #    #  pguser = "cardano";
  #    #  pgdb = "cardano_importer";
  #    #  pghost = "some-aws-host";
  #    #  pgpw = builtins.readFile ../static/cardano-importer-pw;
  #    #};
  #  };
  #};
  adapay = { config, pkgs, resources, ... }: {
    services = {
      #icarus-backend = {
      #  enable = false;
      #};
      #deployment.keys.icarus-backend.keyfile = ../static/icarus-backend-production.js;
      #adapay = {
      #  enable = false;
      #};
      #deployment.keys.adapay.keyfile = ../static/adapay-production.js;
    };
  };
}
