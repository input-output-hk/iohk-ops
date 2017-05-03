with (import ./../lib.nix);

{
  network.description = "IOHK infrastructure";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      # 16G memory
      instanceType = mkForce "r3.large";
      ebsInitialRootDiskSize = mkForce 200;
      associatePublicIpAddress = true;
    };
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      # 16G memory needed for 100 nodes evaluation
      instanceType = mkForce "r3.large";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
      ami = mkForce "ami-01f7306e";
    };
  };
}
