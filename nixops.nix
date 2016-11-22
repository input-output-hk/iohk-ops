let
  lib = (import <nixpkgs> {}).lib;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
  # See secretExample.nix
  secret = import ./secret.nix;
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

    imports = [ ./modules/cardano-node.nix ];
    networking.firewall.enable = false;
  } // lib.optionalAttrs (generatingAMI != "1") {
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = secret.accessKeyId;
    deployment.ec2.instanceType = "t2.large";
    deployment.ec2.securityGroups = [secret.securityGroup];
    deployment.ec2.ebsBoot = true;
    deployment.ec2.ebsInitialRootDiskSize = 6;
  };

  # Move to separate timewarp-node.nix
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
         then toString [
                "/bin/sh -c \"${time-warp}/bin/bench-sender "
                "+RTS -N -RTS "
                "--log-config /home/timewarp/sender-logging.yaml "
                "--logs-prefix /home/timewarp "
                "`cat /home/timewarp/peers.txt` "
                "-r 90 -m 10800 -d 130 \""
              ]
         else toString [
                "${time-warp}/bin/bench-receiver "
                "+RTS -N -RTS "
                "--log-config /home/timewarp/receiver-logging.yaml "
                "--logs-prefix /home/timewarp "
                "-p 3055 -d 150 "
              ];
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
    deployment.ec2.ami = (import ./amis.nix).${region};
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
