with (import ./../lib.nix);

{ pkgs, config, resources, options, ...}:

let
  cfg = config.services.cardano-benchmark;
  iohkpkgs = import ../. {};
  # relies on the commits cleverca22 made in https://github.com/input-output-hk/cardano-sl/commits/6d83230489ff70985ffd6b7c785286d7535566c4
  params = index: {
    topologyFile = cfg.topologyFile;
    walletListen = "127.0.0.1:${toString (8090 + index)}";
    ekgListen = "127.0.0.1:${toString (8000 + index)}";
    environment = "override";
    confKey = cfg.configKey;
    extraParams = "--system-start ${toString cfg.systemStart}";
  };
  mkService = index: {
    "cardano-node-${toString index}" = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "cardano-node-${toString index}";
        WorkingDirectory = "/home/cardano-node-${toString index}";
        ExecStart = iohkpkgs.connectScripts.mainnet.wallet.override (params index);
      };
    };
  };
  mkUser = index: {
    "cardano-node-${toString index}" = {
      isNormalUser = true;
    };
  };
in {
  options = {
    services.cardano-benchmark = {
      walletsPerNode = mkOption {
        type = types.int;
      };
      systemStart = mkOption {
        type = types.int;
      };
      index = mkOption {
        type = types.int;
      };
      topologyFile = mkOption {
        type = types.str;
      };
      configKey = mkOption {
        type = types.str;
      };
    };
  };

  config = {
    systemd.services = mkMerge (map mkService (range 1 cfg.walletsPerNode));
    users.users = mkMerge (map mkUser (range 1 cfg.walletsPerNode));
  };
}
