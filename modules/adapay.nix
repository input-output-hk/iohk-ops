{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.adapay;
  rev = "cb5f8262e3eedec31866d958400cb5f962320a7e";
  # need ssh-agent forwarding to fetch private repo using your ssh key
  adapaySrc = builtins.fetchgit {
    url = "ssh://git@github.com:input-output-hk/summit-AdaPay";
    inherit rev;
    sha256 = "0z5ccs95q05nhnmp16g7nbc2pqqbmsqlriffffyjdzn8jxh7wy7f";
  };
  adapay = import adapaySrc {};
in {
  options.services.adapay = {
    enable = mkEnableOption "enable adapay";
  };
  config = lib.mkIf cfg.enable {
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
        WorkingDirectory = config.users.adapay.home;
      };
      script = ''
        mkdir -p config
        [ -f /run/keys/adapay-production.js ] && cp -f /run/keys/adapay-production.js ./config/production.js
        NODE_ENV=production adapay
      '';
    };
  };
}
