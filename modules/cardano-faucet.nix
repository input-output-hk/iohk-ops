with (import ./../lib.nix);

globals: params:
{ config, pkgs, lib, ...}:

{
  global.dnsHostname = mkForce "cardano-faucet";

  imports = [ ./cardano-faucet-module.nix ];

  services.cardano-faucet = {
    enable = true;
    relayHost = "relays.cardano-testnet.iohkdev.io";
    configKey = "testnet_full";
    statsd.enable = true;
    statsd.port = 8125; # dogstatsd will be listening on this port
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;
    virtualHosts = let
      vhostDomainName = if config.global.dnsDomainname != null
        then config.global.dnsDomainname else "iohkdev.io";
     in {
      "cardano-faucet.${vhostDomainName}" = {
        # TLS provided by cloudfront
        locations = {
          "/" = {
            # root = cardanoPackages.cardano-sl-faucet-frontend;
            # Serve static files or fallback to browser history api
            # tryFiles = "$uri /index.html";
            proxyPass = "http://127.0.0.1:${toString config.services.cardano-faucet.port}";
          };
        };
      };
    };
  };
}
