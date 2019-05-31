{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.cachecache;
  iohkops = import ../. { inherit pkgs; };
in {
  options = {
    services.cachecache.enable = mkEnableOption "enable cachecache";
  };
  config = mkIf cfg.enable {
    users.users.cachecache = {
      home = "/var/lib/cachecache";
      isSystemUser = true;
      createHome = true;
    };
    systemd.services.cachecache = {
      wantedBy = [ "multi-user.target" ];
      path = [ iohkops.cachecache ];
      script = ''
        exec cachecache
      '';
      serviceConfig = {
        User = "cachecache";
        WorkingDirectory = config.users.users.cachecache.home;
      };
    };
  };
}
