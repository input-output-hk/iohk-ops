with (import ./../lib.nix);

let
  cconf = import ./../config.nix;

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
    imports = [ ./../modules/common.nix ];

    services.cardano-node = {
      enable = true;
      port = cconf.nodePort;
      testIndex = testIndex;
      dhtKey = genDhtKey { i = testIndex; };
      stats = false;
      jsonLog = false;
      distribution = true;
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval totalMoneyAmount bitcoinOverFlat productionMode;
    };
  } // optionalAttrs (generatingAMI == false) ({
      deployment.ec2.region = mkForce region;
      deployment.ec2.keyPair = mkForce (keypair resources.ec2KeyPairs);
      deployment.ec2.elasticIPv4 = resources.elasticIPs.${"nodeip" + toString testIndex};
    } // optionalAttrs cconf.productionMode  {
      deployment.keys."key${toString (testIndex + 1)}" = {
        text = builtins.readFile (builtins.getEnv("PWD") + "/keys/key${toString (testIndex + 1)}.sk");
        user = "cardano-node";
      };
  });

  cardano-node-coordinator = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      timeLord = true;
    };
  };

  cardano-node = {testIndex, region, keypair}: {pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];
  };

  regionIndex = region: keypair: testIndex: cardano-node { inherit region testIndex keypair; };
  cardano-node-eu = regionIndex "eu-central-1" (pairs: pairs.cardano-test-eu);
  cardano-node-eu_old = regionIndex "eu-central-1" (pairs: pairs.my-key-pair);
  cardano-node-us = regionIndex "us-west-1" (pairs: pairs.cardano-test-us);
  cardano-node-asia = regionIndex "ap-southeast-1" (pairs: pairs.cardano-test-asia);
  cardano-node-sydney = regionIndex "ap-southeast-2" (pairs: pairs.cardano-test-sydney);
  cardano-node-sa = regionIndex "sa-east-1" (pairs: pairs.cardano-test-sa);
in 
  (genAttrs' (range 1 9) (key: "node${toString key}") (name: cardano-node-eu name)) // 
  (genAttrs' (range 10 19) (key: "node${toString key}") (name: cardano-node-us name)) // 
  (genAttrs' (range 20 29) (key: "node${toString key}") (name: cardano-node-asia name)) //
  (genAttrs' (range 30 39) (key: "node${toString key}") (name: cardano-node-sydney name)) //
  (genAttrs' (range 40 49) (key: "node${toString key}") (name: cardano-node-sa name)) //
  (genAttrs' (range 50 59) (key: "node${toString key}") (name: cardano-node-eu name)) // 
  (genAttrs' (range 60 69) (key: "node${toString key}") (name: cardano-node-us name)) // 
  (genAttrs' (range 70 79) (key: "node${toString key}") (name: cardano-node-asia name)) //
  (genAttrs' (range 80 89) (key: "node${toString key}") (name: cardano-node-sydney name)) //
  (genAttrs' (range 90 99) (key: "node${toString key}") (name: cardano-node-sa name)) //
{
  network.description = "Cardano SL experiments";

  node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; keypair = (pairs: pairs.my-key-pair); };

  report-server = { pkgs, config, lib, resources, ...}: {
    imports = [ ./../modules/common.nix ./../modules/report-server.nix ];

    services.report-server = {
      enable = true;
      port = 5555;
    };

    deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
  };
  
  resources = {
    inherit ec2KeyPairs;
    elasticIPs = 
      # TODO: generalize with node generation
      (genAttrs' (range 0 6) (key: "nodeip${toString key}") (name: { region = "eu-central-1"; inherit accessKeyId; })) //
      (genAttrs' (range 7 13) (key: "nodeip${toString key}") (name: { region = "us-west-1"; inherit accessKeyId; })) //
      { report-server-ip = { inherit region accessKeyId; }; };
  };
}
