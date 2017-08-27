{ IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
{
  network.description = "IOHK infrastructure";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      inherit IOHKaccessKeyId;

      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 200;
      associatePublicIpAddress = true;
    };
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.keys.tarsnap = {
      keyFile = ./../static/tarsnap-cardano-deployer.secret;
      destDir = "/var/lib/keys";
    };

    deployment.ec2 = {
      inherit IOHKaccessKeyId;

      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
    };
  };
}
