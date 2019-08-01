with (import ./../lib.nix);

params:
{
  imports = [
    ./staging.nix
    ./monitoring-exporters.nix
  ];

  global.dnsHostname = if params.typeIsRelay then "cardano-node-${toString params.relayIndex}" else null;
}
