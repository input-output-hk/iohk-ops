with import ../lib.nix;

{ config, pkgs, lib, ... }:

let
  cfg = config.services.explorer-python-api;
  iohkPkgs = import ../default.nix { };
  explorerPythonAPI = iohkPkgs.explorerPythonAPI;
in {
  options = {
    services.explorer-python-api = {
      enable = lib.mkEnableOption "Explorer Python API";
      epochSlots = mkOption {
        description = "The number of slots per epoch. 0 < EPOCHSLOTS <= 21600";
        type = types.ints.positive;
        default = 21600;
      };
      scrapeName = mkOption {
        description = "The nodename.clustername to scrape the python API metrics from.";
        type = types.str;
        default = "localhost";
      };
    };
  };
  config = lib.mkIf cfg.enable {

    services.nginx = lib.mkIf config.services.nginx.enable {
      virtualHosts =
        let vhostDomainName = if config.global.dnsDomainname != null
                              then config.global.dnsDomainname else "iohkdev.io";
        in {
          "cardano-explorer.${vhostDomainName}" = {
            locations = {
              "/api/addresses/summary/".proxyPass = "http://127.0.0.1:7000";
          };
        };
      };
    };
    networking.firewall.allowedTCPPorts = [ 7000 ];
    users.users.explorer-python-api = {
      home = "/var/empty";
      isSystemUser = true;
    };
    systemd.services.explorer-python-api = {
      wantedBy = [ "multi-user.target" ];
      environment = {
        DBSOCKPATH = "/tmp";
        EPOCHSLOTS = "${builtins.toString cfg.epochSlots}";
      };
      preStart = "sleep 5";
      script = "exec ${explorerPythonAPI}/bin/run-explorer-python-api";
      serviceConfig = {
        User = "explorer-python-api";
        Restart = "always";
        RestartSec = "30s";
      };
    };
    services.postgresql = {
      enable = true;
      dataDir = "/var/lib/explorer-python-api";
      enableTCPIP = false;
      initialScript = pkgs.writeText "explorerPythonAPI-initScript" ''
        create database explorer_python_api;
        \connect explorer_python_api;
        create schema scraper;
        create table scraper.blocks (
                                cbeBlkHash        text      primary key
                              , cbeEpoch          smallint
                              , cbeSlot           smallint
                              , cbeBlkHeight      integer
                              , cbeTimeIssued     timestamp without time zone
                              , cbeTxNum          integer
                              , cbeTotalSent      bigint
                              , cbeSize           integer
                              , cbeBlockLead      text
                              , cbeFees           bigint
                              , cbsPrevHash       text
                              , cbsNextHash       text
                              , cbsMerkleRoot     text
                              );
        create table scraper.tx (
                                ctsId               text      primary key
                              , ctsTxTimeIssued     timestamp without time zone
                              , ctsBlockTimeIssued  timestamp without time zone
                              , ctsBlockHash        text
                              , ctsTotalInput       bigint
                              , ctsTotalOutput      bigint
                              , ctsFees             bigint
                              );
        create table scraper.txinput (
                                ctsId               text
                              , ctsIdIndex          smallint
                              , ctsInputAddr        text
                              , ctsInput            bigint
                              , constraint pk_txinput primary key (ctsId, ctsIdIndex)
                              );
        create table scraper.txoutput (
                                ctsId               text
                              , ctsIdIndex          smallint
                              , ctsOutputAddr       text
                              , ctsOutput           bigint
                              , constraint pk_txoutput primary key (ctsId, ctsIdIndex)
                              );
        create user explorer_python_api;
        grant all privileges on database explorer_python_api to explorer_python_api;
        grant all privileges on schema scraper to explorer_python_api;
        grant all privileges on all tables in schema scraper to explorer_python_api;
      '';
      identMap = ''
        explorer-users explorer-python-api explorer_python_api
        explorer-users root explorer_python_api
        explorer-users postgres postgres
      '';
      authentication = ''
        local all all ident map=explorer-users
      '';
    };
  };
}
