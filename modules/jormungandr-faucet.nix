{ config, options, pkgs, lib, ... }:

let
  sources = import ../nix/sources.nix;
  commonLib = import ../lib.nix;
  inherit (lib) mkIf mkForce;
  inherit (commonLib) rust-packages;

  enable = config.params.typeIsFaucet && config.params.nodeImpl == "rust";
  host = config.networking.privateIPv4;
  apiPort = toString (config.services.cardano-node.port + 1);
  faucetPort = config.services.jormungandr-faucet.port;
  vhostDomainName = if config.global.dnsDomainname == null
                    then "iohkdev.io"
                    else config.global.dnsDomainname;

in {
  global.dnsHostname = mkIf enable (mkForce "jormungandr-faucet");

  environment.systemPackages = mkIf (config.params.nodeImpl == "rust") [
    rust-packages.pkgs.jormungandr-cli
  ];

  services.jormungandr-faucet = mkIf enable {
    enable = true;
    lovelacesToGive = 250000;
    jormungandrApi = "http://${host}:${apiPort}/api/v0";
    secondsBetweenRequests = 30;
  };

  networking.firewall = mkIf enable {
    allowedTCPPorts = [ 80 443 ];
  };

  services.nginx = mkIf enable {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    serverTokens = false;

    commonHttpConfig = ''
      log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                        '"$request" $status $body_bytes_sent '
                        '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
      access_log syslog:server=unix:/dev/log x-fwd;
      limit_req_zone $binary_remote_addr zone=faucetPerIP:100m rate=1r/s;

      map $http_origin $origin_allowed {
        default 0;
        https://webdevc.iohk.io 1;
        https://testnet.iohkdev.io 1;
      }

      map $origin_allowed $origin {
        default "";
        1 $http_origin;
      }
    '';

    virtualHosts = {
      "jormungandr-faucet.${vhostDomainName}" = {
        forceSSL = true;
        enableACME = true;

        locations."/" =
          let
            headers = ''
              add_header 'Vary' 'Origin' always;
              add_header 'Access-Control-Allow-Origin' $origin;
              add_header 'Access-Control-Allow-Methods' 'POST, OPTIONS';
              add_header 'Access-Control-Allow-Headers' 'User-Agent,X-Requested-With,Content-Type';
              add_header 'Access-Control-Max-Age' 1728000;
              add_header 'Content-Type' 'text/plain; charset=utf-8';
              add_header 'Content-Length' 0;
            '';
          in {
          extraConfig = ''
            limit_req zone=faucetPerIP;

            if ($request_method = OPTIONS) {
              ${headers}
              return 204;
              break;
            }

            if ($request_method = POST) {
              ${headers}
            }

            proxy_pass http://127.0.0.1:${toString faucetPort};
            proxy_set_header Host $host:$server_port;
            proxy_set_header X-Real-IP $remote_addr;
          '';
        };
      };
    };
  };
}
