{ globals, ... }: with (import ./../lib.nix);

let nodeMap = { inherit (globals.fullMap) report-server; }; in

flip mapAttrs nodeMap
(name: import ./../modules/report-server.nix
       globals
       [])
// {
  options.services.report-server = {
    logsdir = mkOption { type = types.path; default = "/var/lib/report-server"; };
  };
}
