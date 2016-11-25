with (import ./../lib.nix);

let
  accessKeyId = "cardano-deployer";
  cconf = import ./../compileconfig.nix;

  bitcoinOverFlat = false;
  totalMoneyAmount = 60000000;

  coordinatorPort = 3000;
  coordinatorDhtKey = "MHdtsP-oPf7UWly7QuXnLK5RDB8=";

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
    imports = [ ./../modules/common.nix ];

    services.cardano-node = {
      enable = true;
      port = coordinatorPort;
      testIndex = testIndex;
      stats = true;
      jsonLog = true;
      distribution = true;

      inherit (cconf) genesisN slotDuration networkDiameter mpcRelayInterval;
      inherit totalMoneyAmount bitcoinOverFlat ;
    };
  } // optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = region;
    deployment.ec2.keyPair = keypair resources.ec2KeyPairs;
    deployment.ec2.ami = (import ./../modules/amis.nix).${region};
  };

  cardano-node-coordinator = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      timeLord = true;
      peerEnable = false;
#      dhtKey = genDhtKey { i = testIndex; };
      dhtKey = coordinatorDhtKey;
    };
  };

  cardano-node = {testIndex, region, keypair}: {pkgs, ...}@args: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      peerHost = args.nodes.node0.config.networking.privateIPv4 or "NOT SET";
      peerPort = coordinatorPort;
      peerDhtKey = coordinatorDhtKey;
      peerEnable = true;
#      dhtKey = genDhtKey { i = testIndex; };
    };
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
  #(genAttrs' (range 40 49) (key: "node${toString key}") (name: cardano-node-sa name)) //
  #(genAttrs' (range 50 59) (key: "node${toString key}") (name: cardano-node-eu name)) // 
  #(genAttrs' (range 60 69) (key: "node${toString key}") (name: cardano-node-us name)) // 
  #(genAttrs' (range 70 79) (key: "node${toString key}") (name: cardano-node-asia name)) //
  #(genAttrs' (range 80 89) (key: "node${toString key}") (name: cardano-node-sydney name)) //
  #(genAttrs' (range 90 99) (key: "node${toString key}") (name: cardano-node-sa name)) //
{
  node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; keypair = (pairs: pairs.my-key-pair); };

  resources.ec2KeyPairs.my-key-pair = 
    { inherit accessKeyId; region = "eu-central-1"; };
  resources.ec2KeyPairs.cardano-test-eu = 
    { inherit accessKeyId; region = "eu-central-1"; };
  resources.ec2KeyPairs.cardano-test-us = 
    { inherit accessKeyId; region = "us-west-1"; };
  resources.ec2KeyPairs.cardano-test-asia = 
    { inherit accessKeyId; region = "ap-southeast-1"; };
  resources.ec2KeyPairs.cardano-test-sydney = 
    { inherit accessKeyId; region = "ap-southeast-2"; };
  resources.ec2KeyPairs.cardano-test-sa = 
    { inherit accessKeyId; region = "sa-east-1"; };
}
