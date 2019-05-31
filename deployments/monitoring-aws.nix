{ globals, IOHKaccessKeyId, IOHKroute53accessKeyId, ... }:

with import ../lib.nix;
let
  accessKeyId = IOHKaccessKeyId;
  nodeMap = { inherit (globals.fullMap) monitoring; };
  monitoring = nodeMap.monitoring;
  region = monitoring.region;
  org = monitoring.org;
in {
  require = [
    ./security-groups/allow-public-www-https.nix
    ./security-groups/allow-deployer-ssh.nix
    ./security-groups/allow-monitoring-collection.nix
    ./global.nix
    ./monitoring.nix
  ];
  resources = {
    ec2SecurityGroups = {
      "allow-wireguard-in-${region}-${org}" = {
        inherit region accessKeyId;
        description = "wireguard";
        rules = [{
          protocol = "udp";
          fromPort = 51820; toPort = 51820;
          sourceIp = "0.0.0.0/0";
        }];
      };
    };
  };
  monitoring = { config, lib, resources, ... }: {
    deployment = {
      route53.accessKeyId = lib.mkForce IOHKroute53accessKeyId;
      ec2 = {
        securityGroups = (optionals (! config.global.omitDetailedSecurityGroups) [
          resources.ec2SecurityGroups."allow-wireguard-in-${region}-${org}"
          resources.ec2SecurityGroups."allow-monitoring-static-peers-${region}-${org}"
          resources.ec2SecurityGroups."allow-public-www-https-${region}-${org}"
        ]);
        region         = mkForce monitoring.region;
        accessKeyId    = monitoring.accessKeyId;
        keyPair        = resources.ec2KeyPairs.${monitoring.keyPairName};
      };
    };
  };

  resources.elasticIPs = nodesElasticIPs nodeMap;
}
