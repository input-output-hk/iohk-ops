with (import ./../lib.nix);

globals: params:
{ config, pkgs, resources, ... }:

{
  imports = [
    ./datadog.nix
    ./papertrail.nix
  ];

  # DEVOPS-64: disable log bursting
  services.journald.rateLimitBurst = 0;

  services.cardano-node = {
    saveCoreDumps = true;
    serveEkg = true;
  };

  deployment.ec2.elasticIPv4 = resources.elasticIPs.${params.name + "-ip"};

  deployment.route53.accessKeyId = params.accessKeyId;
  deployment.route53.hostName = if params.typeIsRelay
                           then "cardano-node-${toString params.relayIndex}.${(envSpecific globals.environment).dnsSuffix}"
                           else if params.typeIsExplorer
                           then "cardano-explorer.${(envSpecific globals.environment).dnsSuffix}"
                           else "";

  services.dd-agent.tags = ["env:${globals.environment}"];
  services.dd-agent.processConfig = ''
    init_config:

    instances:
    '' +
    (if params.typeIsExplorer
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
