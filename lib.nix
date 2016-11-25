let
  lib = (import <nixpkgs> {}).lib;
in lib // (rec {
  # Allows to also generate the key compared to upstream genAttrs
  genAttrs' = names: fkey: fname:
    lib.listToAttrs (map (n: lib.nameValuePair (fkey n) (fname n)) names);

  # If we're generating an AMI, don't set nixops deployment attributes
  generatingAMI = builtins.getEnv "GENERATING_AMI";

  # Function to generate DHT key
  genDhtKey = { i
              , dhtKeyPrefix  ? "MHdrsP-oPf7UWly"
              , dhtKeyPostfix ? "7QuXnLK5RD=" }:
              let padded =
                  if i < 10
                  then "0" + toString i
                  else toString i
              ; in dhtKeyPrefix + padded + dhtKeyPostfix;

  accessKeyId = "cardano-deployer";
  
  ec2Keys = {
    resources.ec2KeyPairs.my-key-pair = 
      { inherit accessKeyId; region = "eu-central-1"; };
    resources.ec2KeyPairs.cardano-test-eu = 
      { inherit accessKeyId; region = "eu-central-1"; };
    resources.ec2KeyPairs.cardano-test-us = 
      { inherit accessKeyId; region = "us-west-1"; };
    resources.ec2KeyPairs.cardano-test-asia = 
      { inherit accessKeyId; region = "ap-southeast-1"; };
    resources.ec2KeyPairs.cardano-test-sydney = 
      { inherit accessKeyId; region = "ap-southeast-2"; };
    resources.ec2KeyPairs.cardano-test-sa = 
      { inherit accessKeyId; region = "sa-east-1"; };
  };
})
