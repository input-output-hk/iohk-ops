let
  lib = (import <nixpkgs> {}).lib;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
  # See modules/secretExample.nix
  secret = import ./modules/secret.nix;
  genesisN = 20;
  slotDuration = 20;
  bitcoinOverFlat = false;
  totalMoneyAmount = 600000;
  networkDiameter = 7;
  mpcRelayInterval = 10;
  genDhtKey = { i
              , dhtKeyPrefix  ? "MHdrsP-oPf7UWly"
              , dhtKeyPostfix ? "7QuXnLK5RD=" }:
              let padded =
                  if i < 10
                  then "0" + toString i
                  else toString i
              ; in dhtKeyPrefix + padded + dhtKeyPostfix;
  coordinatorHost = "52.59.93.58"; # Elastic
  coordinatorPort = 3000;
  coordinatorDhtKey = "MHdtsP-oPf7UWly7QuXnLK5RDB8=";

  timeWarpNode = import ./modules/timewarp-node.nix;

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
    imports = [ (import ./modules/common.nix) ];

    services.cardano-node = {
      enable = true;
      port = coordinatorPort;
      testIndex = testIndex;
      stats = true;
      jsonLog = true;
      distribution = true;
      pettyUtxo = true;
      isInfo = true;

      inherit genesisN slotDuration totalMoneyAmount bitcoinOverFlat networkDiameter mpcRelayInterval;
    };
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = region;
    deployment.ec2.keyPair = keypair resources.ec2KeyPairs;
    deployment.ec2.ami = (import ./modules/amis.nix).${region};
  };

  cardano-node-coordinator = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      timeLord = true;
      peerEnable = false;
#      dhtKey = genDhtKey { i = testIndex; };
      dhtKey = coordinatorDhtKey;
    };
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.ec2.elasticIPv4 = coordinatorHost;
  };

  cardano-node = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      peerHost = coordinatorHost;
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

  genAttrs' = names: fkey: fname:
    lib.listToAttrs (map (n: lib.nameValuePair (fkey n) (fname n)) names);
in 
  (genAttrs' (lib.range 1 9) (key: "node${toString key}") (name: cardano-node-eu name)) // 
  (genAttrs' (lib.range 10 19) (key: "node${toString key}") (name: cardano-node-us name)) // 
  #(genAttrs' (lib.range 20 29) (key: "node${toString key}") (name: cardano-node-asia name)) //
  #(genAttrs' (lib.range 30 39) (key: "node${toString key}") (name: cardano-node-sydney name)) //
  #(genAttrs' (lib.range 40 49) (key: "node${toString key}") (name: cardano-node-sa name)) //
  #(genAttrs' (lib.range 50 59) (key: "node${toString key}") (name: cardano-node-eu name)) // 
  #(genAttrs' (lib.range 60 69) (key: "node${toString key}") (name: cardano-node-us name)) // 
  #(genAttrs' (lib.range 70 79) (key: "node${toString key}") (name: cardano-node-asia name)) //
  #(genAttrs' (lib.range 80 89) (key: "node${toString key}") (name: cardano-node-sydney name)) //
  #(genAttrs' (lib.range 90 99) (key: "node${toString key}") (name: cardano-node-sa name)) //
{
  #node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; keypair = (pairs: pairs.my-key-pair); };

  # node0 = timeWarpNode { sender = true; };
  # node1 = timeWarpNode { };


  resources.ec2KeyPairs.my-key-pair = 
    { inherit (secret) accessKeyId; region = "eu-central-1"; };
  resources.ec2KeyPairs.cardano-test-eu = 
    { inherit (secret) accessKeyId; region = "eu-central-1"; };
  resources.ec2KeyPairs.cardano-test-us = 
    { inherit (secret) accessKeyId; region = "us-west-1"; };
  resources.ec2KeyPairs.cardano-test-asia = 
    { inherit (secret) accessKeyId; region = "ap-southeast-1"; };
  resources.ec2KeyPairs.cardano-test-sydney = 
    { inherit (secret) accessKeyId; region = "ap-southeast-2"; };
  resources.ec2KeyPairs.cardano-test-sa = 
    { inherit (secret) accessKeyId; region = "sa-east-1"; };
}
