let
  lib = (import <nixpkgs> {}).lib;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
  # See secretExample.nix
  secret = import ./secret.nix;
  genesisN = 40;
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
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = secret.accessKeyId;
    deployment.ec2.instanceType = "t2.large";
    deployment.ec2.securityGroups = [secret.securityGroup];
    deployment.ec2.ebsBoot = true;
    deployment.ec2.ebsInitialRootDiskSize = 8;
  };

  timeWarpNode = { sender ? false }:
   { resources, pkgs, ...}: 
  let
    time-warp = (import ./srk-nixpkgs/default.nix { inherit pkgs; }).time-warp;
  in {
    imports = [ defaultConfig ];

    users = {
      users.timewarp = {
        description     = "";
        group           = "timewarp";
        createHome      = true;
        isNormalUser = true;
      };
      groups.timewarp = { };
    };

    networking.firewall.allowedTCPPorts = [ 3055 ];

    systemd.services.timewarp = {
      description   = "";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];
      serviceConfig = {
        User = "timewarp";
        Group = "timewarp";
        Restart = "always";
        KillSignal = "SIGINT";
        PrivateTmp = true;
        ExecStart =
         if sender
         then "${time-warp}/bin/bench-sender"
         else "${time-warp}/bin/bench-reciever";
      };
    };
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = "eu-central-1";
    deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
  };

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
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
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = region;
    deployment.ec2.keyPair = keypair resources.ec2KeyPairs;
  };

  cardano-node-coordinator = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      timeLord = true;
      peerEnable = false;
      dhtKey = genDhtKey { i = testIndex; };
#      dhtKey = coordinatorDhtKey;
    };
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.ec2.elasticIPv4 = coordinatorHost;
  };

  cardano-node = {testIndex, region, keypair}: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex region keypair) ];

    services.cardano-node = {
      #peerHost = coordinatorHost;
      #peerPort = coordinatorPort;
      #peerDhtKey = coordinatorDhtKey;
      peerEnable = false;
      dhtKey = genDhtKey { i = testIndex; };
    };
  };

  regionIndex = region: keypair: testIndex: cardano-node { inherit region testIndex keypair; };
  cardano-node-eu = regionIndex "eu-central-1" (pairs: pairs.cardano-test-eu);
  cardano-node-eu_old = regionIndex "eu-central-1" (pairs: pairs.my-key-pair);
  cardano-node-us = regionIndex "us-west-1" (pairs: pairs.cardano-test-us);
  cardano-node-asia = regionIndex "ap-southeast-1" (pairs: pairs.cardano-test-asia);
  cardano-node-sydney = regionIndex "ap-southeast-2" (pairs: pairs.cardano-test-sydney);
  cardano-node-sa = regionIndex "sa-east-1" (pairs: pairs.cardano-test-sa);
in {
  node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; keypair = (pairs: pairs.my-key-pair); };
  node1 = cardano-node-eu_old 1; 
  node2 = cardano-node-eu_old 2; 
  node3 = cardano-node-eu_old 3; 
  node4 = cardano-node-eu_old 4; 
  node5 = cardano-node-eu_old 5; 
  node6 = cardano-node-eu_old 6; 
  node7 = cardano-node-eu_old 7; 
  node8 = cardano-node-eu_old 8; 
  node9 = cardano-node-eu_old 9; 
  node10 = cardano-node-eu_old 10; 
  node11 = cardano-node-eu_old 11; 
  node12 = cardano-node-eu_old 12; 
  node13 = cardano-node-eu_old 13; 
# node14 = cardano-node-eu_old 14; 
# node15 = cardano-node-eu_old 15; 
# node16 = cardano-node-eu_old 16; 
# node17 = cardano-node-eu_old 17; 
# node18 = cardano-node-eu_old 18; 
# node19 = cardano-node-eu_old 19; 
# node20 = cardano-node-eu_old 20; 
  node21 = cardano-node-us 14; 
  node22 = cardano-node-us 15; 
  node23 = cardano-node-us 16; 
  node24 = cardano-node-us 17; 
  node25 = cardano-node-us 18; 
  node26 = cardano-node-us 19; 
  node27 = cardano-node-us 20; 
  node28 = cardano-node-us 21; 
  node29 = cardano-node-us 22; 
  node30 = cardano-node-us 23; 
  node31 = cardano-node-us 24; 
  node32 = cardano-node-us 25; 
# node33 = cardano-node-us 33; 
# node34 = cardano-node-us 34; 
# node35 = cardano-node-us 35; 
# node36 = cardano-node-us 36; 
# node37 = cardano-node-us 37; 
# node38 = cardano-node-us 38; 
  node39 = cardano-node-asia 26; 
  node40 = cardano-node-asia 27; 
  node41 = cardano-node-asia 28; 
  node42 = cardano-node-asia 29; 
  node43 = cardano-node-asia 30; 
  node44 = cardano-node-asia 31; 
  node45 = cardano-node-asia 32; 
  node46 = cardano-node-asia 33; 
  node47 = cardano-node-asia 34; 
  node48 = cardano-node-us 35; 
  node49 = cardano-node-asia 36; 
  node50 = cardano-node-asia 37; 
  node51 = cardano-node-asia 38; 
  node52 = cardano-node-asia 39; 
 node53 = cardano-node-asia 53; 
 node54 = cardano-node-asia 54; 
 node55 = cardano-node-asia 55; 
 node56 = cardano-node-asia 56; 
 node57 = cardano-node-asia 57; 
 node58 = cardano-node-asia 58; 
 node59 = cardano-node-asia 59;
 node60 = cardano-node-sydney 60;
 node61 = cardano-node-sydney 61;
 node62 = cardano-node-sydney 62;
 node63 = cardano-node-sydney 63;
 node64 = cardano-node-sydney 64;
 node65 = cardano-node-sydney 65;
 node66 = cardano-node-sydney 66;
 node67 = cardano-node-sydney 67;
 node68 = cardano-node-sydney 68;
 node69 = cardano-node-sydney 69;
 node70 = cardano-node-sydney 70;
 node71 = cardano-node-sydney 71;
 node72 = cardano-node-sydney 72;
 node73 = cardano-node-eu 73;
 node74 = cardano-node-eu 74;
 node75 = cardano-node-eu 75;
 node76 = cardano-node-eu 76;
 node77 = cardano-node-eu 77;
 node78 = cardano-node-eu 78;
 node79 = cardano-node-eu 79;
 node80 = cardano-node-eu 80;
 node81 = cardano-node-eu 81;
 node82 = cardano-node-eu 82;
 node83 = cardano-node-eu 83;
 node84 = cardano-node-eu 84;
 node85 = cardano-node-eu 85;
 node86 = cardano-node-sa 86;
 node87 = cardano-node-sa 87;
 node88 = cardano-node-sa 88;
 node89 = cardano-node-sa 89;
 node90 = cardano-node-sa 90;
 node91 = cardano-node-sa 91;
 node92 = cardano-node-sa 92;
 node93 = cardano-node-sa 93;
 node94 = cardano-node-sa 94;
 node95 = cardano-node-sa 95;
 node96 = cardano-node-sa 96;
 node97 = cardano-node-sa 97;
 node98 = cardano-node-sa 98;
 node99 = cardano-node-sa 99;

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
