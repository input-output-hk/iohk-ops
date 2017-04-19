with (import ./../lib.nix);

let
  conf = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    # DEVOPS-64: disable log bursting
    services.journald.rateLimitBurst = 0;

    # Initial block is big enough to hold 3 months of transactions
    deployment.ec2.ebsInitialRootDiskSize = mkForce 700;

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
  };
in {
  report-server = conf;
  sl-explorer = conf;
} // (genAttrs' (range 0 13) (key: "node${toString key}") (name: conf))
