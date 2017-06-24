{ accessKeyId, ... }:

with (import ./../lib.nix);
{
  network.description = "Cardano SL";

  report-server = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/amazon-base.nix
    ];

    deployment.ec2.accessKeyId = accessKeyId;
  };
}
