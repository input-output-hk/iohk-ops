let
  region = "eu-central-1";
  accessKeyId = "cardano-deployer";          # Doesn't depend on a region
  securityGroup = "cardano-deployment"; # Does depend on a region

  coordinatorHost = "52.59.93.58"; # Elastic
  coordinatorDhtKey = "MHdtsP-oPf7UWly7QuXnLK5RDB8=";

  volhovmKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRMQ16PB/UvIEF+UIHfy66FNaBUWgviE2xuD5qoq/nXURBsHogGzv1ssdj1uaLdh7pZxmo/cRC+Y5f6dallIHHwdiKKOdRq1R/IWToMxnL/TTre+px6rxq21al9r4lvibelIU9vDn0R6OFZo+pRWyXUm33bQ4DVhwWiSls3Hw+9xRq4Pf2aWy//ey5CUTW+QkVdDIOFQG97kHDO3OdoNuaOMdeS+HBgH25bzSlcMw044T/NV9Cyi3y1eEBCoyqA9ba28GIl3vNADBdoQb5YYhBViFLaFsadzgWv5XWTpXV4Kwnq8ekmTcBkDzoTng/QOrDLsFMLo1nEMvhbFZopAfZ volhovm.cs@gmail.com";
  georgeeeKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCymYrIVeNUd9TUPc3cTdHIAatTg3qPbuTENNNHCKQyM4PPvWE+DzmyVDki07NpBk9Ivge3whklcTpRVTMXs7AFX3YIdIxpvc+XVgKhweqd8H0QZkC4/gsJNVTBuY1ZQ2Ldw/rRmbiA9lx/z3vtoI5p4oLSumP2qd5l/KwjDvj66X8K4KOofkFFEiPqBztQwt+A2Hh6XH5qeakQQm/TFeNL6SU0X0zKRdhjyzYAEa2Nt/Te1KK+Jkof7vZ2YnJ3jQFUhC/yRej4o3MPde0HoEP7L86rm9ORcSyQe4jZJ/d6qXMNFAG/7LfU+3LVJ+T584kHXBm5Jl5rOyX2MngNxLxP georgeee@georgeee-laptop";
  gromakKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDK0/mW1SZPMKqWawAthYKBMgLXxCfkzoAMdnQ3cakfFg5QWmmoV6QemtuaPAhBGcA4b10TPh3zW77zYY6ZnSq60iog15iAZxZByYVqBN+7j7JaQHS/cW3uf8KuVc4c/nYDrf3rj8+K0bkllKsfAM3z1JGFvYbO6UPdsjalGwG6GEvkVbbP4y+g5XG58ylgXMdUxvjWBZZTL0Ao8gc5fSeAIjkvVCM/a5EBG1/q7xTukWF2HXqQWc5/551bwDZIkUWsyUm/lr+EUFQBuwXEQ6uwhqA0MFo1r+8ge0eWB0l+fDMn59eBTBlzUNEMjafqOETJZhRU9ieDJUHEsjYVcBd7 gie1994@gmail.com";

  defaultConfig = { resources, pkgs, ... }: {
    environment.systemPackages =
      let
        srk-pkgs = import ./srk-nixpkgs/srk-pkgs.nix { inherit pkgs; };
      in with pkgs; [ git tmux vim nixops srk-pkgs.cardano lsof ];
    users.extraUsers.root.openssh.authorizedKeys.keys = [ volhovmKey georgeeeKey gromakKey ];
    services.openssh.passwordAuthentication = false;
    services.openssh.enable = true;

    imports = [ ./cardano-node.nix ];

    # EC2 stuff
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.securityGroups = [securityGroup];
  };

  nodeGenericConfig = testIndex: {resources, pkgs, ...}: {
    imports = [ defaultConfig ];

    services.cardano-node = {
      enable = true;
      testIndex = testIndex;
    };
 
    networking.firewall.enable = false;
  };

  cardano-node-coordinator = testIndex: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex) ];

    deployment.ec2.elasticIPv4 = coordinatorHost;
    services.cardano-node.timeLord = true;
    services.cardano-node.peerEnable = false;
    services.cardano-node.dhtKey = coordinatorDhtKey;
  };

  cardano-node = testIndex: {resources, pkgs, ...}: {
    imports = [ (nodeGenericConfig testIndex) ];

    services.cardano-node.peerHost = coordinatorHost;
    services.cardano-node.peerDhtKey = coordinatorDhtKey;
  };


in {
  node0-coordinator = cardano-node-coordinator 0;
  node1 = cardano-node 1;
  node2 = cardano-node 2;
  node3 = cardano-node 3;

#  node4 = cardano-node 4;
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
    { inherit region accessKeyId; };
}
