with (import ./../lib.nix);

globals: params:
{ config, pkgs, lib, ...}:

let
  recaptcha = {
    siteKey = lib.fileContents ../static/recaptcha_site_key;
    secretKeyFile = "/var/lib/keys/recaptcha-secret-key";
  };
  iohkPkgs = import ../default.nix { inherit config pkgs; inherit (pkgs) system; };

  faucetFrontend = iohkPkgs.makeFaucetFrontend {
    explorerURL = "http://cardano-explorer.cardano-testnet.iohkdev.io/";
    recaptchaSiteKey = recaptcha.siteKey;
  };

  explorerURL = "http://cardano-explorer.cardano-testnet.iohkdev.io/";

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

  deployment.keys.recaptcha-secret-key = {
    keyFile = ../static/recaptcha_secret_key;
    destDir = "/var/lib/keys";
    user = "cardano-faucet";
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    virtualHosts = let
      vhostDomainName = if config.global.dnsDomainname != null
        then config.global.dnsDomainname else "iohkdev.io";
     in {
      "cardano-faucet.${vhostDomainName}" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = faucetFrontend;
            tryFiles = "$uri $uri/index.html @faucet";
          };
          "@faucet" = {
            proxyPass = "http://127.0.0.1:${toString config.services.cardano-faucet.port}";
          };
        };
        # Otherwise nginx serves files with timestamps unixtime+1 from /nix/store
        extraConfig = ''
          if_modified_since off;
          add_header Last-Modified "";
          etag off;
        '';
      };
    };
  };
}
