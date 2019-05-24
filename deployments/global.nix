{ globals, domain, ... }:

{
  defaults = { lib, ... }: {
    _file = ./global.nix;
    imports = [ ../modules/globals.nix ];
    global = {
      inherit (globals) environment systemStart topologyYaml nodeMap nRelays relays;
    } // lib.optionalAttrs (domain != null) {
      dnsDomainname = domain;
    };

  };
}
