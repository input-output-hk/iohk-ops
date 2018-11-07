{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.log-classifier;
    log-classifier-src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "log-classifier";
      rev = "aaf0fb346655d93a41bc982569e934176d1b14b7";
      sha256 = "0q438s6mcwbgnnma8ndh493cpq0f9i46j7svnph7kmj5123k16lc";
    };
    log-classifier-web = (import "${log-classifier-src}/release.nix").log-classifier-web;

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
    services.nginx = {
      enable = true;
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
