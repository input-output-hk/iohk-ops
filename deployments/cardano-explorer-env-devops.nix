{ globals, ... }: with (import ../lib.nix);
let
  accessKeyId = IOHKaccessKeyId;
  nodeMap = { inherit (globals.fullMap) explorer; };
  explorer = nodeMap.explorer;
  region = explorer.region;
  org = explorer.org;

in {

  require = [
    ./security-groups/allow-explorer-python-monitoring.nix
    ./global.nix
  ];

  explorer = { config, lib, resources, ... }: ( (import ../modules/cardano-devops.nix) nodeMap.explorer ) //
  {
    deployment = {
      # route53.accessKeyId = lib.mkForce IOHKroute53accessKeyId;
      ec2 = {
        securityGroups = (optionals (! config.global.omitDetailedSecurityGroups) [
          resources.ec2SecurityGroups."allow-explorer-python-monitoring-${region}-${org}"
        ]);
        region         = explorer.region;
        accessKeyId    = mkForce explorer.accessKeyId;
        # keyPair        = resources.ec2KeyPairs.${explorer.keyPairName};
      };
    };
    services.explorer-python-api = {
      enable = true;
      epochSlots = 60;
    };
  };

  resources = {
    elasticIPs = nodesElasticIPs nodeMap;
  };

  monitoring = { nodes, pkgs, ... }:
  {
    services.prometheus2.scrapeConfigs = [
      {
        job_name = "explorer-api-monitor";
        scrape_interval = "60s";
        metrics_path = "/metrics";
        static_configs = [
          {
            targets = [
              "explorer.${nodes.explorer.config.deployment.name}:7000"
            ];
            labels = { alias = "explorer-python-api"; };
          }
        ];
      }
    ];
  };
}
