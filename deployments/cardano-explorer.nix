with (import ./../lib.nix);

{
  network.description = "Cardano SL explorer node";

  sl-explorer = { config, pkgs, ... }: {
    imports = [
      ./../modules/common.nix
    ];

    services.cardano-node = {
      enable = true;
      executable = "${(import ./../default.nix {}).cardano-sl-explorer-static}/bin/cardano-explorer";
      port = cconf.nodePort;
      testIndex = 40;
      dhtKey = genDhtKey { i = 40; };
      stats = false;
      jsonLog = false;
      distribution = true;
      autoStart = true;
      # TODO: this should/may come with cardano-sl and we would just use it from there
      peersFile = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/input-output-hk/daedalus/cardano-sl-0.3/installers/data/ip-dht-mappings";
        sha256 = "0ycppdril21arvxiw0xjs65xnv957jdx31jhq1ar9dz7hna0jidg";
      };
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval totalMoneyAmount bitcoinOverFlat productionMode;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.nginx = {
      enable = true;
      virtualHosts = {
        "cardano-explorer-dev.iohk.io" = {
          # TLS provided by cloudfront
          locations = {
            # TODO: one day we'll build purescript with Nix!
            # but today, this is built by ./scripts/generate-explorer-frontend.sh
            "/".root = ./../cardano-sl-explorer/frontend/dist;
            "/api/".proxyPass = "http://localhost:8100";
          };
        };
      };
    };
  };
}
