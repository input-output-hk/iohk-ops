{ IOHKaccessKeyId, ... }:

with (import ./../lib.nix);
{
  network.description = "IOHK infrastructure production";

  hydra = { config, pkgs, resources, ... }: {

    imports = [
      ./../modules/papertrail.nix
      ./../modules/datadog.nix
    ];

    services.dd-agent.tags = ["env:production"];
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:production"];

  };
}
