let
  # See secretExample.nix
  secret = import ./secret.nix;
  genesisN = 5;

  coordinatorHost = "52.59.93.58"; # Elastic
  coordinatorPort = 2000;
  coordinatorDhtKey = "MHdtsP-oPf7UWly7QuXnLK5RDB8=";

  defaultConfig = { resources, pkgs, ... }: {
    environment.systemPackages =
      let
        srk-nixpkgs = import ./srk-nixpkgs/default.nix { inherit pkgs genesisN; };
      in with pkgs; [ git tmux vim sysstat nixops srk-nixpkgs.cardano-sl lsof ];
    users.extraUsers.root.openssh.authorizedKeys.keys = secret.devKeys;
    services.openssh.passwordAuthentication = false;
    services.openssh.enable = true;

    environment.variables.TERM = "xterm-256color";

    imports = [ ./cardano-node.nix ];

    # EC2 stuff
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = secret.accessKeyId;
    deployment.ec2.region = secret.region;
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.securityGroups = [secret.securityGroup];
  };

  nodeGenericConfig = testIndex: {resources, pkgs, ...}: {
    imports = [ defaultConfig ];

    services.cardano-node = {
      enable = true;
      testIndex = testIndex;
      pettyUtxo = true;
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
  node0-coordinator = cardano-node-coordinator 0;
  node1 = cardano-node 1;
  node2 = cardano-node 2;
  node3 = cardano-node 3;
  node4 = cardano-node 4;
#  node5 = cardano-node 5;
#  node6 = cardano-node 6;
#  node7 = cardano-node 7;
#  node8 = cardano-node 8;
#  node9 = cardano-node 9;
#  node10 = cardano-node 10;
#  node11 = cardano-node 11;
#  node12 = cardano-node 12;
#  node13 = cardano-node 13;
#  node14 = cardano-node 14;
#  node15 = cardano-node 15;
#  node16 = cardano-node 16;
#  node17 = cardano-node 17;
#  node18 = cardano-node 18;
#  node19 = cardano-node 19;
#  node20 = cardano-node 20;
#  node21 = cardano-node 21;
#  node22 = cardano-node 22;
#  node23 = cardano-node 23;
#  node24 = cardano-node 24;
#  node25 = cardano-node 25;
#  node26 = cardano-node 26;
#  node27 = cardano-node 27;
#  node28 = cardano-node 28;
#  node29 = cardano-node 29;
#  node30 = cardano-node 30;
#  node31 = cardano-node 31;
#  node32 = cardano-node 32;
#  node33 = cardano-node 33;
#  node34 = cardano-node 34;
#  node35 = cardano-node 35;
#  node36 = cardano-node 36;
#  node37 = cardano-node 37;
#  node38 = cardano-node 38;
#  node39 = cardano-node 39;
#  node40 = cardano-node 40;

  resources.ec2KeyPairs.my-key-pair = 
    { inherit (secret) region accessKeyId; };
}
