{ config, pkgs, lib, resources, ... }:

with (import ./../lib.nix);

optionalAttrs (generatingAMI == false) {
 deployment.targetEnv = "ec2";
 deployment.ec2.instanceType = "t2.large";
 deployment.ec2.region = region;
 deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu-central;
 deployment.ec2.securityGroups = ["cardano-deployment"];
 deployment.ec2.ami = (import ./../modules/amis.nix).${config.deployment.ec2.region};
 deployment.ec2.accessKeyId = "cardano-deployer";
 deployment.ec2.ebsInitialRootDiskSize = 30;
}
