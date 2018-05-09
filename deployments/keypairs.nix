{ globals, ... }:

with import ../lib.nix;
{
  resources.ec2KeyPairs =
    (listToAttrs (flip map globals.orgXRegions
                 ({ org, region }:
                  nameValuePair (orgRegionKeyPairName org region)
                                { inherit region;
                                  accessKeyId = globals.orgAccessKeys.${org};
                                  description = "Keypair for ${org}/${region}";
                                }))) //
    {
      cardano-test-eu         = { accessKeyId = globals.orgAccessKeys.IOHK; region = "eu-central-1"; };
      cardano-test-eu-central = { accessKeyId = globals.orgAccessKeys.IOHK; region = "eu-central-1"; };
    };

  # Create GC generation on deployer machine
  network.enableRollback = true;
}
