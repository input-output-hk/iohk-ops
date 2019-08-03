{ globals, monitorIpOverride ? null, ... }:

let

  mkRule = { org, region }: {
    name = "allow-explorer-python-monitoring-${region}-${org}";
    value = { nodes, resources, lib, ... }:
    let
      monitoringSourceIp = resources.elasticIPs."${globals.monitoringNV.name}-ip" or monitorIpOverride;
    in {
      inherit region;
      _file = ./allow-explorer-python-monitoring.nix;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "Monitoring Explorer Python";
      rules = lib.optionals (nodes ? "${globals.monitoringNV.name}" || monitorIpOverride != null)
      [
        {
          protocol = "tcp";
          fromPort = 7000; toPort = 7000; # prometheus exporters
          sourceIp = monitoringSourceIp;
        }
        {
          protocol = "tcp";
          fromPort = 7001; toPort = 7001; # prometheus exporters
          sourceIp = monitoringSourceIp;
        }
      ];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}
