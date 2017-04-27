with (import ./../lib.nix);

let
  cconf = import ./../config.nix;

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
    imports = [
      ./../modules/common.nix
      ./../modules/amazon-base.nix
    ];

    services.cardano-node = {
      enable = true;
      port = cconf.nodePort;
      testIndex = testIndex;
      dhtKey = genDhtKey { i = testIndex; };
      stats = false;
      jsonLog = true;
      distribution = true;
      peersFile = pkgs.fetchurl {
         url = "https://raw.githubusercontent.com/input-output-hk/daedalus/cardano-sl-0.4/installers/data/ip-dht-mappings";
         sha256 = "02nlxx28r1fc3ncc1qmzvxv6vlmy1i8rll10alr6fh9qf9nxwzbv";
       };
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval totalMoneyAmount bitcoinOverFlat productionMode systemStart richPoorDistr;
    };
  } // optionalAttrs (generatingAMI == false) ({
      deployment.ec2.region = mkForce region;
      deployment.ec2.keyPair = mkForce (keypair resources.ec2KeyPairs);
      deployment.ec2.elasticIPv4 = resources.elasticIPs.${"nodeip" + toString testIndex};
    } // optionalAttrs cconf.productionMode  {
      #deployment.keys."key${toString (testIndex + 1)}" = {
      #  text = builtins.readFile (builtins.getEnv("PWD") + "/keys/key${toString (testIndex + 1)}.sk");
      #  user = "cardano-node";
      #};
  });

  cardano-node = {testIndex, region, keypair}: {pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];
  };

  regionIndex = region: keypair: testIndex: cardano-node { inherit region testIndex keypair; };
in {
  network.description = "Cardano SL explorer";

  sl-explorer = { pkgs, config, lib, resources, ... }: {
    imports = [
      (nodeGenericConfig 40 "eu-central-1" (pairs: pairs.cardano-test-eu-central))
    ];

    services.cardano-node = {
      executable = "${(import ./../default.nix {}).cardano-sl-explorer-static}/bin/cardano-explorer";
      autoStart = true;
    };

    networking.firewall.allowedTCPPorts = [
      80 #nginx
      8110
    ];

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

  resources = {
    inherit ec2KeyPairs;
    elasticIPs =
      {
        nodeip40 = { inherit region accessKeyId; };
      };
  };
}
