{ name, config, lib, resources, ... }:
let inherit (config.node) region org;
in
{
  imports = [  ./amazon-base.nix ];
  ec2.hvm = true;
  deployment.ec2 = {
    ebsInitialRootDiskSize = lib.mkForce 200;
    associatePublicIpAddress = true;
    securityGroups = [
      resources.ec2SecurityGroups."allow-deployer-ssh-${region}-${org}"
      resources.ec2SecurityGroups."allow-hydra-${region}-${org}"
    ];
  };
}
