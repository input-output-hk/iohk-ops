with (import ./../lib.nix);

{ config, pkgs, resources, ... }: {
  imports = [
    ./datadog.nix
    ./papertrail.nix
    ./common.nix
  ];

  # DEVOPS-64: disable log bursting
  services.journald.rateLimitBurst = 0;

  services.cardano-node = {
    saveCoreDumps = true;
    serveEkg = true;
  };

  deployment.ec2.elasticIPv4 = resources.elasticIPs.${toString config.services.cardano-node.nodeName + "-ip"};

  deployment.route53.accessKeyId = config.deployment.ec2.accessKeyId;
  deployment.route53.hostName = "cardano-node-${toString config.services.cardano-node.nodeIndex}.aws.iohkdev.io";

  services.dd-agent.tags = ["env:staging"];
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
