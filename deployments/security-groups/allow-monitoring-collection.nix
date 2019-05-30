{ globals, ... }:

let

  mkRule = { org, region }: {
    name = "allow-monitoring-collection-${region}-${org}";
    value = { nodes, resources, ... }: {
      inherit region;
      _file = ./allow-monitoring-collection.nix;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "Monitoring collection";
      rules = if nodes ? "${globals.monitoringNV.name}" then
        (let monitoringSourceIp = resources.elasticIPs."${globals.monitoringNV.name}-ip";
      in [{
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
      }]) else [];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}
