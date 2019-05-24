{ globals, ... }:

{
  defaults = {
    _file = ./global.nix;
    imports = [ ../modules/globals.nix ];
    global = {
      inherit (globals) environment systemStart topologyYaml nodeMap nRelays relays;
    };

    nixpkgs.overlays = [
      (import ../overlays/monitoring-exporters.nix)
    ];
  };
}
