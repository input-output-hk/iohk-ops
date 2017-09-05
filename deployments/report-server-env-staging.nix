{ globals, ... }: with (import ./../lib.nix);

let params = globals.fullMap.report-server;
in
{
  report-server = { config, resources, ...}: {
    imports = [
      ./../modules/staging.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];
  };
}
