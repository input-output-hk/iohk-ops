{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.adapay;
  rev = "4dc5b17652174ca6584f9a6a4aea0c82a74764a9";
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
        NODE_ENV=${cfg.environment} exec adapay
      '';
    };
  };
}
