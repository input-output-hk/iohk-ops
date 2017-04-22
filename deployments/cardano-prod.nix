with (import ./../lib.nix);

let
  conf = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];
    services.dd-agent.tags = ["env:production"];

    # DEVOPS-64: disable log bursting
    services.journald.rateLimitBurst = 0;

    # Initial block is big enough to hold 3 months of transactions
    deployment.ec2.ebsInitialRootDiskSize = mkForce 700;
  };
in {
  report-server = conf;
} // (genAttrs' (range 0 13) (key: "node${toString key}") (name: conf))
