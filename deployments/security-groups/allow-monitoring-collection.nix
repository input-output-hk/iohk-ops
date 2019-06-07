{ globals, monitorIpOverride ? null, ... }:

let

  mkRule = { org, region }: {
    name = "allow-monitoring-collection-${region}-${org}";
    value = { nodes, resources, lib, ... }:
    let
      monitoringSourceIp = resources.elasticIPs."${globals.monitoringNV.name}-ip" or monitorIpOverride;
    in {
      inherit region;
      _file = ./allow-monitoring-collection.nix;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "Monitoring collection";
      rules = lib.optionals (nodes ? "${globals.monitoringNV.name}" || monitorIpOverride != null)
        [{
          protocol = "tcp";
          fromPort = 9100; toPort = 9100; # prometheus exporters
          sourceIp = monitoringSourceIp;
        }{
          protocol = "tcp";
          fromPort = 9102; toPort = 9102; # statd exporter
          sourceIp = monitoringSourceIp;
        }{
          protocol = "tcp";
          fromPort = 9113; toPort = 9113; # nginx exporter
          sourceIp = monitoringSourceIp;
        }];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}
