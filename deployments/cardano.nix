with (import ./../lib.nix);

let
  cconf = import ./../config.nix;

  nodeGenericConfig = testIndex: region: keypair: {resources, pkgs, ...}: {
    imports = [
      ./../modules/common.nix
      ./../modules/amazon-base.nix
    ];

    services.cardano-node = {
      enable = true;
      port = cconf.nodePort;
      testIndex = testIndex;
      dhtKey = genDhtKey { i = testIndex; };
      stats = false;
      jsonLog = true;
      distribution = true;
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval totalMoneyAmount bitcoinOverFlat productionMode systemStart richPoorDistr;
    };
  } // optionalAttrs (generatingAMI == false) ({
      deployment.ec2.region = mkForce region;
      deployment.ec2.keyPair = mkForce (keypair resources.ec2KeyPairs);
      deployment.ec2.elasticIPv4 = resources.elasticIPs.${"nodeip" + toString testIndex};
      # Initial block is big enough to hold 3 months of transactions
      deployment.ec2.ebsInitialRootDiskSize = mkForce 700;
    } // optionalAttrs cconf.productionMode  {
      #deployment.keys."key${toString (testIndex + 1)}" = {
      #  text = builtins.readFile (builtins.getEnv("PWD") + "/keys/key${toString (testIndex + 1)}.sk");
      #  user = "cardano-node";
      #};
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

  cardano-node-eu-central = regionIndex "eu-central-1" (pairs: pairs.cardano-test-eu-central);
  cardano-node-eu-west-1 = regionIndex "eu-west-1" (pairs: pairs.cardano-test-eu-west-1);
  cardano-node-eu-west-2 = regionIndex "eu-west-2" (pairs: pairs.cardano-test-eu-west-2);
  cardano-node-ap-southeast-1 = regionIndex "ap-southeast-1" (pairs: pairs.cardano-test-ap-southeast-1);
  cardano-node-ap-southeast-2 = regionIndex "ap-southeast-2" (pairs: pairs.cardano-test-ap-southeast-2);
  cardano-node-ap-northeast-1 = regionIndex "ap-northeast-1" (pairs: pairs.cardano-test-ap-northeast-1);
  cardano-node-ap-northeast-2 = regionIndex "ap-northeast-2" (pairs: pairs.cardano-test-ap-northeast-2);

in
  # NOTE: Also update resources at the bottom
  (genAttrs' (range 1 1) (key: "node${toString key}") (name: cardano-node-eu-central name)) //
  (genAttrs' (range 2 3) (key: "node${toString key}") (name: cardano-node-eu-west-1 name)) //
  (genAttrs' (range 4 5) (key: "node${toString key}") (name: cardano-node-eu-west-2 name)) //
  (genAttrs' (range 6 7) (key: "node${toString key}") (name: cardano-node-ap-southeast-1 name)) //
  (genAttrs' (range 8 9) (key: "node${toString key}") (name: cardano-node-ap-southeast-2 name)) //
  (genAttrs' (range 10 11) (key: "node${toString key}") (name: cardano-node-ap-northeast-1 name)) //
  (genAttrs' (range 12 13) (key: "node${toString key}") (name: cardano-node-ap-northeast-2 name)) //
{
  network.description = "Cardano SL experiments";

  node0 = cardano-node-coordinator { testIndex = 0; region = "eu-central-1"; keypair = (pairs: pairs.cardano-test-eu-central); };

  report-server = { pkgs, config, lib, resources, ...}: {
    imports = [
      ./../modules/common.nix
      ./../modules/amazon-base.nix
      ./../modules/report-server.nix
    ];

    services.report-server = {
      enable = true;
    };

    deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
  };

  resources = {
    inherit ec2KeyPairs;
    elasticIPs =
      # TODO: generalize with node generation
      (genAttrs' (range 0 1) (key: "nodeip${toString key}") (name: { region = "eu-central-1"; inherit accessKeyId; })) //
      (genAttrs' (range 2 3) (key: "nodeip${toString key}") (name: { region = "eu-west-1"; inherit accessKeyId; })) //
      (genAttrs' (range 4 5) (key: "nodeip${toString key}") (name: { region = "eu-west-2"; inherit accessKeyId; })) //
      (genAttrs' (range 6 7) (key: "nodeip${toString key}") (name: { region = "ap-southeast-1"; inherit accessKeyId; })) //
      (genAttrs' (range 8 9) (key: "nodeip${toString key}") (name: { region = "ap-southeast-2"; inherit accessKeyId; })) //
      (genAttrs' (range 10 11) (key: "nodeip${toString key}") (name: { region = "ap-northeast-1"; inherit accessKeyId; })) //
      (genAttrs' (range 12 13) (key: "nodeip${toString key}") (name: { region = "ap-northeast-2"; inherit accessKeyId; })) //
      { report-server-ip = { inherit region accessKeyId; }; };
  };
}
