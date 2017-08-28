{ config, ... }:

{
  imports = [ ./datadog.nix ];
  services.dd-agent.tags = ["env:scaling"];
  services.dd-agent.processConfig = ''
    init_config:

    instances:
    - name: cardano-node-simple
      search_string: ['cardano-node-simple']
      exact_match: True
      thresholds:
        critical: [1, 1]
  '';
}
