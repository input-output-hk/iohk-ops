{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.adapay;
  rev = "19cc4830c6f6ef72ad87324dea9f0a0c25eba420";
  ref = "0.1.2";
  # need ssh-agent forwarding to fetch private repo using your ssh key
  adapaySrc = builtins.fetchGit {
    url = "ssh://git@github.com/input-output-hk/summit-AdaPay";
    inherit rev ref;
  };
  adapay = (import adapaySrc).adapay;
in {
  options.services.adapay = {
    enable = mkEnableOption "enable adapay";
    environment = mkOption {
      description = "environment";
      type = types.str;
    };
  };
  config = mkIf cfg.enable {
    users.users.adapay = {
      home = "/var/lib/adapay";
      createHome = true;
      isSystemUser = true;
      extraGroups = [ "keys" ];
    };
    systemd.services = let
      makeAdapayWorker = name: {
        after = [ "adapay.service" ];
        partOf = [ "adapay.service" ];
        wantedBy = [ "multi-user.target" ];
        path = [ adapay ];
        serviceConfig = {
          User = "adapay";
          WorkingDirectory = config.users.users.adapay.home;
        };
        script = ''
          NODE_ENV=${cfg.environment} exec adapay-${name}-worker
        '';
      };
    in {
      adapay = {
        wantedBy = [ "multi-user.target" ];
        path = [ adapay ];
        after = [ "adapay-${cfg.environment}.js-key.service" ];
        partOf = [ "adapay-${cfg.environment}.js-key.service" ];
        serviceConfig = {
          User = "adapay";
          WorkingDirectory = config.users.users.adapay.home;
        };
        script = ''
          mkdir -p config
          [ -f /run/keys/adapay-${cfg.environment}.js ] && cp -f /run/keys/adapay-${cfg.environment}.js ./config/${cfg.environment}.js
          ln -svf ${adapay}/node_modules/adapay/src
          NODE_ENV=${cfg.environment} exec adapay
        '';
      };
      adapay-price-worker = makeAdapayWorker "price";
      adapay-promo-code-gen-worker = makeAdapayWorker "promo-code-gen";
      adapay-status-updater-worker = makeAdapayWorker "status-updater";
    };
  };
}
