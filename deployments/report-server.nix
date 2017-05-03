with (import ./../lib.nix);

{
  report-server = import ./../modules/report-server.nix;
}
