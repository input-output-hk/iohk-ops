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

  monitoring = { nodes, ... }:
  {
    imports = [
      ../modules/monitoring-services.nix
    ];

    services = {
      prometheus2.scrapeConfigs = [{
        job_name = "explorer-api-monitor";
        scrape_interval = "60s";
        metrics_path = "/metrics";
        static_configs = [
          {
            targets = [
              "explorer.${nodes.explorer.config.deployment.name}:7000"
              "explorer.${nodes.explorer.config.deployment.name}:7001"
            ];
            labels = { alias = "explorer-python-api"; };
          }
        ];
      }];
      monitoring-services = {
        applicationRules = [
          {
            alert = "exchange-python-down";
            expr = "rate(block_height[5m]) == 0";
            for = "10m";
            labels.severity = "page";
            annotations = {
              summary = "{{$labels.alias}} blockheight unchanged for >=10mins";
              description = "{{$labels.alias}} blockheight unchanged for >=10mins.  Under ordinary operation, we would expect blockheight to increase by 3 blocks per minute.";
            };
          }
        ];
      };
    };
  };
}
