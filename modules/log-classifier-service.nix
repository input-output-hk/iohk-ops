{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.log-classifier;
  iohkops = import ../. { inherit pkgs; };
  inherit (iohkops) log-classifier-web;
in {
  options.services.log-classifier = {
    enable = mkEnableOption "enable log classifier";
    domain = mkOption {
      description = "Domain to host under";
      type = types.str;
    };
    secrets = mkOption {
      description = "Environment variable secrets";
      type = types.path;
    };
  };
  config = mkIf cfg.enable {
    users.users.log-classifier = {
      description = "log-classifier User";
      group = "log-classifier";
      home = "/var/lib/log-classifier";
      createHome = true;
      isSystemUser = true;
    };
    users.groups.log-classifier = { };
    environment.systemPackages = with pkgs; [ goaccess ];
    services.nginx = {
      enable = true;
      commonHttpConfig = ''
        log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                         '"$request" $status $body_bytes_sent '
                         '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
        access_log syslog:server=unix:/dev/log x-fwd;
      '';
      virtualHosts."${cfg.domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8100";
        };
        extraConfig = ''
          if_modified_since off;
          add_header Last-Modified "";
          etag off;
        '';
      };
      eventsConfig = ''
        worker_connections 1024;
      '';

      appendConfig = ''
        worker_processes 4;
        worker_rlimit_nofile 2048;
      '';
    };
    systemd.services."log-classifier" = {
      wantedBy = [ "multi-user.target" ];
      path = [ log-classifier-web ];
      script = "exec log-classifier-web";
      serviceConfig = {
        User = "log-classifier";
        EnvironmentFile = cfg.secrets;
        WorkingDirectory = config.users.users.log-classifier.home;
      };
    };
  };
}
