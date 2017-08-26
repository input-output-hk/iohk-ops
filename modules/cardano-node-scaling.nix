{ ... }:

{
  imports = [ ./datadog.nix ];
  services.dd-agent.tags = ["env:scaling"];
  services.dd-agent.processConfig = ''
    init_config:

    instances:
    '' +
    (if config.services.cardano-node.nodeName == "explorer"
    then ''
    - name: cardano-explorer
      search_string: ['cardano-explorer']
      ''
    else ''
    - name: cardano-node-simple
      search_string: ['cardano-node-simple']
      '') + ''
      exact_match: True
      thresholds:
        critical: [1, 1]
  '';
}
