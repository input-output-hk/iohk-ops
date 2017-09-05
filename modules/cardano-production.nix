with (import ./../lib.nix);

params:
{ name, config, pkgs, resources, ... }: {
  imports = [
    ./production.nix
    ./datadog.nix
    ./papertrail.nix
  ];

  global.dnsHostname = if params.typeIsRelay then "cardano-node-${toString params.relayIndex}" else null;

  # Initial block is big enough to hold 3 months of transactions
  deployment.ec2.ebsInitialRootDiskSize = mkForce 700;

  services.dd-agent.processConfig = ''
    init_config:

    instances:
    - name: cardano-node
      search_string: ['cardano-node']
      exact_match: False
      thresholds:
        critical: [1, 1]
  '';
}
