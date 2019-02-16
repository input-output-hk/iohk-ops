{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.cardano-importer;
  rev = "c0c903a3d809e9554d60f4379167b938a3ef0720";
  cardanoSrc = pkgs.fetchFromGitHub {
    owner = "Emurgo";
    repo = "project-icarus-importer";
    inherit rev;
    sha256 = "0580v3zdlf24dlnsf1zksvqbrkldc1483n73164rdibig83h69y2";
  };
  environments = {
    staging = {
      confKey = "mainnet_dryrun_full";
      relay = "relays.awstest.iohkdev.io";
    };
    production = {
      confKey = "mainnet_full";
      relay = "relays.cardano-mainnet.iohk.io";
    };
  };
  env = environments.${cfg.environment};
  cardano = import cardanoSrc { gitrev = rev; };
  topofile = pkgs.writeText "topology.yaml" ''
    wallet:
      relays: [[{ host: ${env.relay}]]
      valency: 1
      fallbacks: 7
  '';
in {
  options.services.cardano-importer = {
    enable = mkEnableOption "enable cardano importer";
    environment = mkOption {
      description = "environment";
      type = types.str;
    };
    pguser = mkOption {
      description = "postgres user";
      type = types.str;
    };
    pgdb = mkOption {
      description = "postgres db";
      type = types.str;
    };
    pghost = mkOption {
      description = "postgres host";
      type = types.str;
    };
    pgpwFile = mkOption {
      description = "postgres pw file";
      type = types.path;
    };
  };
  config = lib.mkIf cfg.enable {
    users.users.cardano = {
      home = "/var/lib/cardano-importer";
      createHome = true;
      isSystemUser = true;
    };
    systemd.services.cardano-importer = {
      wantedBy = [ "multi-user.target" ];
      path = [ cardano.cardano-sl-blockchain-importer-static ];
      serviceConfig = {
        User = "cardano";
        WorkingDirectory = config.users.users.cardano.home;
      };
      script = let pgPassFile = toString cfg.pgpwFile;
      in ''
        [ -f ${pgPassFile} ] && cp -f ${pgPassFile} pg-pw

        exec cardano-blockchain-importer --configuration-key ${env.confKey} --configuration-file ${cardanoSrc}/lib/configuration.yaml --topology ${topofile} --statsd-server 127.0.0.1:8125 --metrics +RTS -T -RTS --postgres-user ${cfg.pguser} --postgres-name ${cfg.pgdb} --postgres-pass $(cat pg-pw)
      '';
    };
  };
}
