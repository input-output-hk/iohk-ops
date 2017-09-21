{ globals, ... }:

with import ./lib.nix;
{
  resources.ec2KeyPairs =
    listToAttrs (flip map globals.orgXRegions
                 ({ org, region }:
                  nameValuePair (orgRegionKeyPairName org region)
                                { inherit region;
                                  accessKeyId = globals.orgAccessKeys.${org};
                                  description = "Keypair for ${org}/${region}";
                                }));

  # Create GC generation on deployer machine
  network.enableRollback = true;
}
