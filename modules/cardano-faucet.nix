with (import ./../lib.nix);

globals: params:
{ config, pkgs, lib, ...}:

let
  recaptcha = {
    siteKey = builtins.readFile ../static/recaptcha-site-key;
    secretKey = builtins.readFile ../static/recaptcha-secret-key;
  };
  explorerURL = "http://cardano-explorer.cardano-testnet.iohkdev.io/";
  faucetFrontend = pkgs.runCommand "faucet-frontend" {} ''
    mkdir -p $out
    sed -e 's|recaptchaSiteKey:.*|recaptchaSiteKey: "${recaptcha.siteKey}",|g' \
        -e 's|explorerURL:.*|explorerURL: "${explorerURL}",|g' \
        ${../lib/faucet.html} > $out/index.html
  '';

in {
  global.dnsHostname = mkForce "cardano-faucet";

  imports = [ ./cardano-faucet-module.nix ];

  services.cardano-faucet = {
    enable = true;
    relayHost = "relays.cardano-testnet.iohkdev.io";
    configKey = "testnet_full";
    statsd.enable = true;
    statsd.port = 8125; # dogstatsd will be listening on this port
    inherit recaptcha;
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
            root = faucetFrontend;
            tryFiles = "$uri $uri/index.html @faucet";
          };
          "@faucet" = {
            proxyPass = "http://127.0.0.1:${toString config.services.cardano-faucet.port}";
          };
        };
      };
    };
  };
}
