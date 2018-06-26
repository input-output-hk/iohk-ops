{ resources, config, pkgs, lib, nodes, ... }:

with lib;

let
in {

  nix = {
    buildMachines = [
      { hostName = "localhost";
        system = "x86_64-linux,builtin";
        maxJobs = 8;
        supportedFeatures = ["kvm" "nixos-test"];
      }
    ];
  };

  services.hydra.hydraURL = "https://hydra-mantis.iohk.io";

  services.nginx = {
    virtualHosts = {
      "hydra-mantis.iohk.io" = {
        forceSSL = true;
        enableACME = true;
        locations."/".extraConfig = ''
          proxy_pass http://127.0.0.1:8080;
          proxy_set_header Host $http_host;
          proxy_set_header REMOTE_ADDR $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto https;
        '';
      };
    };
    commonHttpConfig = ''
      server_names_hash_bucket_size 64;
      keepalive_timeout   70;
      gzip            on;
      gzip_min_length 1000;
      gzip_proxied    expired no-cache no-store private auth;
      gzip_types      text/plain application/xml application/javascript application/x-javascript text/javascript text/xml text/css;
    '';
  };
}
