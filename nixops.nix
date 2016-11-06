let
  # See secretExample.nix
  secret = import ./secret.nix;
  genesisN = 100;

  coordinatorHost = "52.59.93.58"; # Elastic
  coordinatorPort = 2000;
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

    # EC2 stuff
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = secret.accessKeyId;
    deployment.ec2.region = secret.region;
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.instanceType = "t2.small";
    deployment.ec2.securityGroups = [secret.securityGroup];
  };

  nodeGenericConfig = testIndex: {resources, pkgs, ...}: {
    imports = [ defaultConfig ];

    services.cardano-node = {
      enable = true;
      testIndex = testIndex;
      pettyUtxo = false;
      inherit genesisN;
    };
 
    networking.firewall.enable = false;
  };

  cardano-node-coordinator = testIndex: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex) ];

    deployment.ec2.elasticIPv4 = coordinatorHost;
    services.cardano-node = {
      port = coordinatorPort;
      timeLord = true;
      #supporter = false;
      peerEnable = false;
      dhtKey = coordinatorDhtKey;
    };
  };

  cardano-node = testIndex: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex) ];

    services.cardano-node = {
      peerHost = coordinatorHost;
      peerPort = coordinatorPort;
      peerDhtKey = coordinatorDhtKey;
    };
  };


in {
  node0 = cardano-node-coordinator 0;
  node1 = cardano-node 1;
  node2 = cardano-node 2;
  node3 = cardano-node 3;
  node4 = cardano-node 4;
  node5 = cardano-node 5;
  node6 = cardano-node 6;
  node7 = cardano-node 7;
  node8 = cardano-node 8;
  node9 = cardano-node 9;
  node10 = cardano-node 10;
  node11 = cardano-node 11;
  node12 = cardano-node 12;
  node13 = cardano-node 13;
  node14 = cardano-node 14;
  node15 = cardano-node 15;
  node16 = cardano-node 16;
  node17 = cardano-node 17;
  node18 = cardano-node 18;
  node19 = cardano-node 19;
  node20 = cardano-node 20;
  node21 = cardano-node 21;
  node22 = cardano-node 22;
  node23 = cardano-node 23;
  node24 = cardano-node 24;
  node25 = cardano-node 25;
  node26 = cardano-node 26;
  node27 = cardano-node 27;
  node28 = cardano-node 28;
  node29 = cardano-node 29;
  node30 = cardano-node 30;
  node31 = cardano-node 31;
  node32 = cardano-node 32;
  node33 = cardano-node 33;
  node34 = cardano-node 34;
  node35 = cardano-node 35;
  node36 = cardano-node 36;
  node37 = cardano-node 37;
  node38 = cardano-node 38;
  node39 = cardano-node 39;
  node40 = cardano-node 40;
  node41 = cardano-node 41;
  node42 = cardano-node 42;
  node43 = cardano-node 43;
  node44 = cardano-node 44;
  node45 = cardano-node 45;
  node46 = cardano-node 46;
  node47 = cardano-node 47;
  node48 = cardano-node 48;
  node49 = cardano-node 49;
  node50 = cardano-node 50;
  node51 = cardano-node 51;
  node52 = cardano-node 52;
  node53 = cardano-node 53;
  node54 = cardano-node 54;
  node55 = cardano-node 55;
  node56 = cardano-node 56;
  node57 = cardano-node 57;
  node58 = cardano-node 58;
  node59 = cardano-node 59;
  node60 = cardano-node 60;
  node61 = cardano-node 61;
  node62 = cardano-node 62;
  node63 = cardano-node 63;
  node64 = cardano-node 64;
  node65 = cardano-node 65;
  node66 = cardano-node 66;
  node67 = cardano-node 67;
  node68 = cardano-node 68;
  node69 = cardano-node 69;
  node70 = cardano-node 70;
  node71 = cardano-node 71;
  node72 = cardano-node 72;
  node73 = cardano-node 73;
  node74 = cardano-node 74;
  node75 = cardano-node 75;
  node76 = cardano-node 76;
  node77 = cardano-node 77;
  node78 = cardano-node 78;
  node79 = cardano-node 79;
  node80 = cardano-node 80;
  node81 = cardano-node 81;
  node82 = cardano-node 82;
  node83 = cardano-node 83;
  node84 = cardano-node 84;
  node85 = cardano-node 85;
  node86 = cardano-node 86;
  node87 = cardano-node 87;
  node88 = cardano-node 88;
  node89 = cardano-node 89;
  node90 = cardano-node 90;
  node91 = cardano-node 91;
  node92 = cardano-node 92;
  node93 = cardano-node 93;
  node94 = cardano-node 94;
  node95 = cardano-node 95;
  node96 = cardano-node 96;
  node97 = cardano-node 97;
  node98 = cardano-node 98;
  node99 = cardano-node 99;


  resources.ec2KeyPairs.my-key-pair = 
    { inherit (secret) region accessKeyId; };
}
