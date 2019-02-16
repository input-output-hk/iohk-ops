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
  cardano = import cardanoSrc { gitrev = rev; };
  topofile = pkgs.writeText "topology.yaml" ''
    wallet:
      relays: [[{ host: relays.cardano-mainnet.iohk.io }]]
      valency: 1
      fallbacks: 7
  '';
in {
  options.services.cardano-importer = {
    enable = mkEnableOption "enable cardano importer";
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
    pgpw = mkOption {
      description = "postgres pw";
      type = types.str;
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
      path = [ cardano.cardano-sl-blockchain-importer ];
      serviceConfig = {
        User = "cardano";
        WorkingDirectory = config.users.users.cardano.home;
      };
      script = ''
        cardano-blockchain-importer --configuration-key mainnet_full --configuration-file ${cardanoSrc}/lib/configuration.yaml --topology ${topofile} --statsd-server 127.0.0.1:8125 --metrics +RTS -T -RTS --postgres-user sam --postgres-name sam
      '';
    };
  };
}
