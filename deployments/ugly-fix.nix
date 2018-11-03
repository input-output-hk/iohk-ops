{
  hydra = { resources, ... }: {
    deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
  };

  resources.ec2KeyPairs.cardano-test-eu = {
    accessKeyId = "cardano-deployer";
    region = "eu-central-1";
  };
  resources.ec2KeyPairs.cardano-test-eu-central = {
    accessKeyId = "cardano-deployer";
    region = "eu-central-1";
  };
}
