with (import ./../lib.nix);

let
  conf = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];
    services.dd-agent.tags = ["prod"];
  };
in {
  report-server = conf;
} // (genAttrs' (range 0 13) (key: "node${toString key}") (name: conf))
