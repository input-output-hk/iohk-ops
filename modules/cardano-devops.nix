with (import ./../lib.nix);

params:
{
  imports = [
    ./devops.nix
    ./monitoring-exporters.nix
  ];

  global.dnsHostname = if params.typeIsRelay then "cardano-node-${toString params.relayIndex}" else null;
}
