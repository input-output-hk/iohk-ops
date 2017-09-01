with (import ./../lib.nix);

{ name, config, resources, ... }:

{
 deployment.targetEnv = "ec2";
 deployment.ec2.instanceType = mkDefault "t2.large";
 deployment.ec2.region  = mkDefault centralRegion;
 deployment.ec2.keyPair = mkDefault (orgRegionKeyPairName "IOHK" "${config.deployment.ec2.region}");
 deployment.ec2.ebsInitialRootDiskSize = mkDefault 30;
 networking.hostName = "${config.deployment.name}.${config.deployment.targetEnv}.${name}";
}
