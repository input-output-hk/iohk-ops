{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.icarus-backend;
  rev = "fca8a15ab90f342180f33a403f169cf136cfb52c";
  icarusBackendSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "project-icarus-backend-service";
    inherit rev;
    sha256 = "1xclwd5pdxhqfr4mga9qdmqm3m8zc5dblynw8kbq4zd12r2kk20p";
  };
  icarusBackend = (import icarusBackendSrc).project-icarus-backend;
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
        WorkingDirectory = config.users.users.icarus-backend.home;
      };
      script = ''
        mkdir -p config
        [ -f /run/keys/icarus-backend-production.js ] && cp -f /run/keys/icarus-backend-production.js ./config/production.js
        NODE_ENV=production exec project-icarus-backend
      '';
    };
  };
}
