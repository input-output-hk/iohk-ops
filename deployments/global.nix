{ globals, ... }:

{
  defaults = {
    global = {
      inherit (globals) environment systemStart topologyYaml nodeMap nRelays relays;
    };

    nixpkgs.overlays = [
      (import ../overlays/monitoring-exporters.nix)
      (import ../overlays/prometheus-bump.nix)
    ];
  };
}
