{ globals, ... }: with (import ./../lib.nix);

let params = globals.fullMap.report-server;
in
{
  report-server = { resources, ...}: {
    imports = [
      ./../modules/production.nix
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];
  };
}
