with (import ./../lib.nix);

{ config, pkgs, resources, ... }: {
  imports = [
    ./datadog.nix
    ./papertrail.nix
    ./common.nix
  ];

  # DEVOPS-64: disable log bursting
  services.journald.rateLimitBurst = 0;

  deployment.ec2.elasticIPv4 = resources.elasticIPs.${"nodeip" + toString config.services.cardano-node.testIndex};

  services.dd-agent.tags = ["env:staging"];
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
