with (import ./../lib.nix);

params:
{ name, config, pkgs, resources, ... }: {
  imports = [
    ./staging.nix
    ./monitoring-exporters.nix
    ./papertrail.nix
  ];
  
  global.dnsHostname = if params.typeIsRelay then "cardano-node-${toString params.relayIndex}" else null;
}
