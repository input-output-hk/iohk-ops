{ globals, ... }:

{
  require = [ ./report-server-bucket-storage.nix ];
  report-server = import ./../modules/report-server.nix globals.fullMap.report-server;
}
