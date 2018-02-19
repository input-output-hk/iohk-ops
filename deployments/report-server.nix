{ globals, ... }:

{
  report-server = import ./../modules/report-server.nix globals.fullMap.report-server globals.environment;
}
