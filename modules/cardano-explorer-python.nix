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
      addrMaxLen = mkOption {
        description = "The maximum address length: 200 <= ADDRMAXLEN <= 8000";
        type = types.ints.positive;
        default = 200;
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
        ADDRMAXLEN = "${builtins.toString cfg.addrMaxLen}";
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
      extraConfig = ''
        # Optimized for:
        # DB Version: 9.6
        # OS Type: linux
        # DB Type: web
        # Total Memory (RAM): 8 GB
        # Data Storage: ssd

        # Suggested optimization for
        # other configurations can be
        # found at:
        # https://pgtune.leopard.in.ua/

        max_connections = 200
        shared_buffers = 2GB
        effective_cache_size = 6GB
        maintenance_work_mem = 512MB
        checkpoint_completion_target = 0.7
        wal_buffers = 16MB
        default_statistics_target = 100
        random_page_cost = 1.1
        effective_io_concurrency = 200
        work_mem = 10485kB
        min_wal_size = 1GB
        max_wal_size = 2GB
      '';
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
        create index i_blocks_cbeBlkHash on scraper.blocks (cbeBlkHash asc);

        create table scraper.tx (
                                ctsId               text      primary key
                              , ctsTxTimeIssued     timestamp without time zone
                              , ctsBlockTimeIssued  timestamp without time zone
                              , ctsBlockHash        text
                              , ctsTotalInput       bigint
                              , ctsTotalOutput      bigint
                              , ctsFees             bigint
                              );
        create index i_tx_ctsId on scraper.tx (ctsId asc);
        create index i_tx_ctsTxTimeIssued on scraper.tx (ctsTxTimeIssued asc);

        create table scraper.txinput (
                                ctsId               text
                              , ctsIdIndex          smallint
                              , ctsTxTimeIssued     timestamp without time zone
                              , ctsInputAddr        text
                              , ctsInput            bigint
                              , constraint pk_txinput primary key (ctsId, ctsIdIndex)
                              );
        create index i_txinput_ctsId on scraper.txinput (ctsId asc);
        create index i_txinput_ctsIdIndex on scraper.txinput (ctsIdIndex asc);
        create index i_txinput_ctsTxTimeIssued on scraper.txinput (ctsTxTimeIssued asc);
        create index i_txinput_ctsInputAddr_ctsId on scraper.txinput (ctsInputAddr asc, ctsId asc);

        create table scraper.txoutput (
                                ctsId               text
                              , ctsIdIndex          smallint
                              , ctsTxTimeIssued     timestamp without time zone
                              , ctsOutputAddr       text
                              , ctsOutput           bigint
                              , constraint pk_txoutput primary key (ctsId, ctsIdIndex)
                              );
        create index i_txoutput_ctsId on scraper.txoutput (ctsId asc);
        create index i_txoutput_ctsIdIndex on scraper.txoutput (ctsIdIndex asc);
        create index i_txoutput_ctsTxTimeIssued on scraper.txoutput (ctsTxTimeIssued asc);
        create index i_txoutput_ctsOutputAddr_ctsId on scraper.txoutput (ctsOutputAddr asc, ctsId asc);

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
