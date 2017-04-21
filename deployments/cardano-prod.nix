with (import ./../lib.nix);

let
  conf = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];
    services.dd-agent.tags = ["prod"];

    # DEVOPS-64: disable log bursting
    services.journald.rateLimitBurst = 0;
  };
in {
  report-server = conf;
} // (genAttrs' (range 0 13) (key: "node${toString key}") (name: conf))
