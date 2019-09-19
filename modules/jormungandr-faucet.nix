{ config, pkgs, lib, ... }:

let
  sources = import ../nix/sources.nix;
  inherit (lib) mkIf mkForce;

  enable = config.params.typeIsFaucet && config.params.nodeImpl == "rust";
  host = config.networking.privateIPv4;
  apiPort = toString (config.services.cardano-node.port + 1);
  faucetPort = config.services.jormungandr-faucet.port;
  vhostDomainName = if config.global.dnsDomainname == null
                    then "iohkdev.io"
                    else config.global.dnsDomainname;
in {
  global.dnsHostname = mkIf enable (mkForce "jormungandr-faucet");
  services.jormungandr-faucet = mkIf enable {
    enable = true;
    lovelacesToGive = 250000;
    jormungandrApi = "http://${host}:${apiPort}/api/v0";
  };

  networking.firewall = mkIf enable {
    allowedTCPPorts = [ 80 443 faucetPort ];
  };

  services.nginx = mkIf enable {
    enable = true;
    commonHttpConfig = ''
      log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                        '"$request" $status $body_bytes_sent '
                        '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
        access_log syslog:server=unix:/dev/log x-fwd;
    '';
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    serverTokens = false;
    virtualHosts = {
      "jormungandr-faucet.${vhostDomainName}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "http://127.0.0.1:${toString faucetPort}";
      };
    };
  };
}
