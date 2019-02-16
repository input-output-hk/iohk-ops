{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.adapay;
  rev = "13bc67066d3ea3d74f58487b95d659323d4d10fa";
  ref = "nix";
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
