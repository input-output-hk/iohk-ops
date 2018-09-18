{ config, pkgs, ... }: {
  imports = [ ../modules/bors-ng-service.nix ];

  services.bors-ng = {
    enable = true;
    dockerImage = "borsng/bors-ng:latest";
    databaseURL = "postgresql://bors-ng:bors-ng@localhost:5432/bors_ng";
  };
  systemd.services.bors-ng = {
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
  };

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    initialScript = pkgs.writeText "initial.sql" ''
      CREATE USER "bors-ng" PASSWORD 'bors-ng' SUPERUSER;
      CREATE DATABASE bors_ng OWNER "bors-ng";
    '';
    authentication = ''
      # allow access to bors_ng database from docker container
      # TYPE  DATABASE    USER        CIDR-ADDRESS          METHOD
      host    bors_ng     bors-ng     127.0.0.1/8           md5
      host    bors_ng     bors-ng     ::1/128               md5
    '';
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "bors-ng.aws.iohkdev.io" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.bors-ng.port}";
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
