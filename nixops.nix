let
  # See secretExample.nix
  secret = import ./secret.nix;
  genesisN = 60;
  slotDuration = 20;
  bitcoinOverFlat = true;
  totalMoneyAmount = 600000;
  networkDiameter = 6;
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

  defaultConfig = { resources, pkgs, ... }: {
    environment.systemPackages =
      let
        srk-nixpkgs = import ./srk-nixpkgs/default.nix { inherit pkgs genesisN; };
      in with pkgs; [ git tmux vim sysstat nixops srk-nixpkgs.cardano-sl lsof ];

    users.mutableUsers = false;
    users.users.root.openssh.authorizedKeys.keys = secret.devKeys;
    services.openssh.passwordAuthentication = true;
    services.openssh.enable = true;

    users.users.statReader = {
      isNormalUser = true;
      password = secret.rootPassword;
    };

    environment.variables.TERM = "xterm-256color";

    services.cron.enable = true;
    services.cron.systemCronJobs = [
      "*/1 * * * *  root /run/current-system/sw/lib/sa/sadc -S DISK 2 29 /var/log/saALL"
    ];

    imports = [ ./cardano-node.nix ];
    networking.firewall.enable = false;

    # EC2 stuff
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = secret.accessKeyId;
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.instanceType = "t2.large";
    deployment.ec2.securityGroups = [secret.securityGroup];
  };

  nodeGenericConfig = testIndex: region: {resources, pkgs, ...}: {
    imports = [ defaultConfig ];

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
 
    deployment.ec2.region = region;
  };

  cardano-node-coordinator = {testIndex, region}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region) ];

    deployment.ec2.elasticIPv4 = coordinatorHost;
    services.cardano-node = {
      timeLord = true;
      peerEnable = false;
      dhtKey = genDhtKey { i = testIndex; };
#      dhtKey = coordinatorDhtKey;
    };
  };

  cardano-node = {testIndex, region}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region) ];

    services.cardano-node = {
      #peerHost = coordinatorHost;
      #peerPort = coordinatorPort;
      #peerDhtKey = coordinatorDhtKey;
      peerEnable = false;
      dhtKey = genDhtKey { i = testIndex; };
    };
  };

  regionIndex = region: testIndex: cardano-node { inherit region testIndex; };
  cardano-node-eu = regionIndex "eu-central-1";
in {
  node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; };
  node1 = cardano-node-eu 1; 
  node2 = cardano-node-eu 2; 
  node3 = cardano-node-eu 3; 
  node4 = cardano-node-eu 4; 
  node5 = cardano-node-eu 5; 
  node6 = cardano-node-eu 6; 
  node7 = cardano-node-eu 7; 
  node8 = cardano-node-eu 8; 
  node9 = cardano-node-eu 9; 
  node10 = cardano-node-eu 10; 
  node11 = cardano-node-eu 11; 
  node12 = cardano-node-eu 12; 
  node13 = cardano-node-eu 13; 
  node14 = cardano-node-eu 14; 
  node15 = cardano-node-eu 15; 
  node16 = cardano-node-eu 16; 
  node17 = cardano-node-eu 17; 
  node18 = cardano-node-eu 18; 
  node19 = cardano-node-eu 19; 
  node20 = cardano-node-eu 20; 
  node21 = cardano-node-eu 21; 
  node22 = cardano-node-eu 22; 
  node23 = cardano-node-eu 23; 
  node24 = cardano-node-eu 24; 
  node25 = cardano-node-eu 25; 
  node26 = cardano-node-eu 26; 
  node27 = cardano-node-eu 27; 
  node28 = cardano-node-eu 28; 
  node29 = cardano-node-eu 29; 
  node30 = cardano-node-eu 30; 
  node31 = cardano-node-eu 31; 
  node32 = cardano-node-eu 32; 
  node33 = cardano-node-eu 33; 
  node34 = cardano-node-eu 34; 
  node35 = cardano-node-eu 35; 
  node36 = cardano-node-eu 36; 
  node37 = cardano-node-eu 37; 
  node38 = cardano-node-eu 38; 
  node39 = cardano-node-eu 39; 
  node40 = cardano-node-eu 40; 
  node41 = cardano-node-eu 41; 
  node42 = cardano-node-eu 42; 
  node43 = cardano-node-eu 43; 
  node44 = cardano-node-eu 44; 
  node45 = cardano-node-eu 45; 
  node46 = cardano-node-eu 46; 
  node47 = cardano-node-eu 47; 
  node48 = cardano-node-eu 48; 
  node49 = cardano-node-eu 49; 
  node50 = cardano-node-eu 50; 
  node51 = cardano-node-eu 51; 
  node52 = cardano-node-eu 52; 
  node53 = cardano-node-eu 53; 
  node54 = cardano-node-eu 54; 
  node55 = cardano-node-eu 55; 
  node56 = cardano-node-eu 56; 
  node57 = cardano-node-eu 57; 
  node58 = cardano-node-eu 58; 
  node59 = cardano-node-eu 59;
#  node60 = cardano-node 60 "eu-central-1";
#  node61 = cardano-node 61 "eu-central-1";
#  node62 = cardano-node 62 "eu-central-1";
#  node63 = cardano-node 63 "eu-central-1";
#  node64 = cardano-node 64 "eu-central-1";
#  node65 = cardano-node 65 "eu-central-1";
#  node66 = cardano-node 66 "eu-central-1";
#  node67 = cardano-node 67 "eu-central-1";
#  node68 = cardano-node 68 "eu-central-1";
#  node69 = cardano-node 69 "eu-central-1";
#  node70 = cardano-node 70 "eu-central-1";
#  node71 = cardano-node 71 "eu-central-1";
#  node72 = cardano-node 72 "eu-central-1";
#  node73 = cardano-node 73 "eu-central-1";
#  node74 = cardano-node 74 "eu-central-1";
#  node75 = cardano-node 75 "eu-central-1";
#  node76 = cardano-node 76 "eu-central-1";
#  node77 = cardano-node 77 "eu-central-1";
#  node78 = cardano-node 78 "eu-central-1";
#  node79 = cardano-node 79 "eu-central-1";
#  node80 = cardano-node 80 "eu-central-1";
#  node81 = cardano-node 81 "eu-central-1";
#  node82 = cardano-node 82 "eu-central-1";
#  node83 = cardano-node 83 "eu-central-1";
#  node84 = cardano-node 84 "eu-central-1";
#  node85 = cardano-node 85 "eu-central-1";
#  node86 = cardano-node 86 "eu-central-1";
#  node87 = cardano-node 87 "eu-central-1";
#  node88 = cardano-node 88 "eu-central-1";
#  node89 = cardano-node 89 "eu-central-1";
#  node90 = cardano-node 90 "eu-central-1";
#  node91 = cardano-node 91 "eu-central-1";
#  node92 = cardano-node 92 "eu-central-1";
#  node93 = cardano-node 93 "eu-central-1";
#  node94 = cardano-node 94 "eu-central-1";
#  node95 = cardano-node 95 "eu-central-1";
#  node96 = cardano-node 96 "eu-central-1";
#  node97 = cardano-node 97 "eu-central-1";
#  node98 = cardano-node 98 "eu-central-1";
#  node99 = cardano-node 99 "eu-central-1";


  resources.ec2KeyPairs.my-key-pair = 
    { inherit (secret) region accessKeyId; };
}
