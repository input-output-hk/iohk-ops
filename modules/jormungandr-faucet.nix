with import ../lib.nix;
{ config, pkgs, lib }:

let
  enable = config.params.typeIsFaucet && config.params.nodeImpl == "rust";
  host = config.networking.privateIPv4;
  apiPort = toString (config.services.cardano-node.port + 1);
  faucetPort = toString config.services.jormungandr-faucet.port;
  vhostDomainName = lib.attrByPath [ "global" "dnsDomainname" ] "iohkdev.io" config;
in {
  services.jormungandr-faucet = mkIf enable {
    enable = true;
    lovelacesToGive = 250000;
    jormungandrApi = "http://${host}:${apiPort}/api/v0";
  };

  services.nginx = mkIf enable {
    enable = true;
    commonHttpConfig = ''
      log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                        '"$request" $status $body_bytes_sent '
                        '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
        access_log syslog:server=unix:/dev/log x-fwd;
    '';
    virtualHosts = {
      "jormungandr-faucet.${vhostDomainName}" = {
        forceSSL = true;
        enableACME = true;
        listen = [{
          addr = "0.0.0.0";
          port = 80;
        }];
        locations."/".proxyPass = "http://${host}:${faucetPort}";
      };
    };
  };
}
