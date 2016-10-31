let
  region = "eu-central-1";
  accessKeyId = "rscoin-guest-user";         # Doesn't depend on a region
  securityGroup = "rscoin-deploy-sec-group"; # Does depend on a region

  volhovmKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRMQ16PB/UvIEF+UIHfy66FNaBUWgviE2xuD5qoq/nXURBsHogGzv1ssdj1uaLdh7pZxmo/cRC+Y5f6dallIHHwdiKKOdRq1R/IWToMxnL/TTre+px6rxq21al9r4lvibelIU9vDn0R6OFZo+pRWyXUm33bQ4DVhwWiSls3Hw+9xRq4Pf2aWy//ey5CUTW+QkVdDIOFQG97kHDO3OdoNuaOMdeS+HBgH25bzSlcMw044T/NV9Cyi3y1eEBCoyqA9ba28GIl3vNADBdoQb5YYhBViFLaFsadzgWv5XWTpXV4Kwnq8ekmTcBkDzoTng/QOrDLsFMLo1nEMvhbFZopAfZ volhovm.cs@gmail.com";

  coordinatorHost = "52.59.93.58"; # Elastic
  coordinatorDhtKey = "MHdtsP-oPf7UWly7QuXnLK5RDB8=";

  defaultConfig = { resources, pkgs, ... }: {
    environment.systemPackages =
      let
        srk-pkgs = import ./srk-nixpkgs/srk-pkgs.nix { inherit pkgs; };
      in with pkgs; [ git tmux vim nixops srk-pkgs.cardano lsof ];
    users.extraUsers.root.openssh.authorizedKeys.keys = [ volhovmKey ];
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

  nodeGenericConfig = {resources, pkgs, ...}: {
    imports = [ defaultConfig ];

    services.cardano-node = {
      enable = true;
    };
  };

  cardano-node-coordnator = {resources, pkgs, ...}: {
    imports = [ nodeGenericConfig ];

    deployment.ec2.elasticIPv4 = coordinatorHost;
    services.cardano-node.timeLord = true;
    services.cardano-node.peerEnable = false;
    services.cardano-node.dhtKey = coordinatorDhtKey;
  };

  cardano-node = {resources, pkgs, ...}: {
    imports = [ nodeGenericConfig ];

    services.cardano-node.peerHost = coordinatorHost;
    services.cardano-node.peerDhtKey = coordinatorDhtKey;
  };


in {
  node0-coordnator = cardano-node-coordnator;
  node1 = cardano-node;
  node2 = cardano-node;
  node3 = cardano-node;

  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };
}
