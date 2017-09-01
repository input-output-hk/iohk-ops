{ IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
rec {
  hydra = { config, pkgs, resources, name, ... }: {

    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2 = {
      accessKeyId = IOHKaccessKeyId;

      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 200;
      associatePublicIpAddress = true;
    };
    deployment.route53.accessKeyId = config.deployment.ec2.accessKeyId;
    deployment.route53.hostName = "${name}.aws.iohkdev.io";
  };

  hydra-build-slave-1 = hydra;
  hydra-build-slave-2 = hydra;

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.keys.tarsnap = {
      keyFile = ./../static/tarsnap-cardano-deployer.secret;
      destDir = "/var/lib/keys";
    };

    deployment.ec2 = {
      accessKeyId = IOHKaccessKeyId;

      instanceType = mkForce "r3.2xlarge";
      ebsInitialRootDiskSize = mkForce 50;
      associatePublicIpAddress = true;
    };
  };
}
