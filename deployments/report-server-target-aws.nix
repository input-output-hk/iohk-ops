{ IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
{
  network.description = "Cardano SL";

  report-server = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2.accessKeyId = IOHKaccessKeyId;
    deployment.ec2.securityGroups = [
      resources.ec2SecurityGroups."allow-deployer-ssh-${config.deployment.ec2.region}"
      resources.ec2SecurityGroups."allow-to-report-server-${config.deployment.ec2.region}"
    ];
  };
}
