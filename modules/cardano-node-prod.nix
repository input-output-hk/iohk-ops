with (import ./../lib.nix);

{ config, pkgs, resources, ... }: {
  imports = [
    ./datadog.nix
    ./papertrail.nix
  ];

  # DEVOPS-64: disable log bursting
  services.journald.rateLimitBurst = 0;

  # Initial block is big enough to hold 3 months of transactions
  deployment.ec2.ebsInitialRootDiskSize = mkForce 700;

  deployment.ec2.elasticIPv4 = resources.elasticIPs.${toString config.services.cardano-node.nodeName + "-ip"};

  deployment.route53.accessKeyId = config.deployment.ec2.accessKeyId;
  deployment.route53.hostName = optionalString (config.services.cardano-node.type == "relay")
                                "cardano-node-${toString config.services.cardano-node.relayIndex}.aws.iohk.io";

  services.dd-agent.tags = ["env:production"];
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
