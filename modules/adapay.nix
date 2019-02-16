{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.adapay;
  rev = "e5344e068a2c7b0081afc7acd3c9d21ca0da5fe6";
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
    systemd.services.adapay = {
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
  };
}
