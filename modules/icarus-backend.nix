{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.icarus-backend;
  rev = "9f965ef1fc58e5d79006e9ef405039f5f28dce63";
  icarusBackendSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "project-icarus-importer";
    inherit rev;
    sha256 = "0z5ccs95q05nhnmp16g7nbc2pqqbmsqlriffffyjdzn8jxh7wy7f";
  };
  icarusBackend = import icarusBackendSrc {};
in {
  options.services.icarus-backend = {
    enable = mkEnableOption "enable icarus-backend";
  };
  config = lib.mkIf cfg.enable {
    users.users.icarus-backend = {
      home = "/var/lib/icarus-backend";
      createHome = true;
      isSystemUser = true;
      extraGroups = [ "keys" ];
    };
    systemd.services.icarus-backend = {
      wantedBy = [ "multi-user.target" ];
      path = [ icarusBackend ];
      serviceConfig = {
        User = "icarus-backend";
        WorkingDirectory = config.users.icarus-backend.home;
      };
      script = ''
        mkdir -p config
        [ -f /run/keys/icarus-backend-production.js ] && cp -f /run/keys/icarus-backend-production.js ./config/production.js
        NODE_ENV=production icarus-backend
      '';
    };
  };
}
