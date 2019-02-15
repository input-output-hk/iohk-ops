{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.cardano-importer;
  rev = "cb5f8262e3eedec31866d958400cb5f962320a7e";
  cardanoSrc = pkgs.fetchFromGitHub {
    owner = "Emurgo";
    repo = "project-icarus-importer";
    inherit rev;
    sha256 = "0z5ccs95q05nhnmp16g7nbc2pqqbmsqlriffffyjdzn8jxh7wy7f";
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
        WorkingDirectory = config.users.cardano.home;
      };
      script = ''
        cardano-blockchain-importer --configuration-key mainnet_full --configuration-file ${cardano.cardano-sl-config}/lib/configuration.yaml --topology ${topofile} --statsd-server 127.0.0.1:8125 --metrics +RTS -T -RTS --postgres-user sam --postgres-name sam
      '';
    };
  };
}
