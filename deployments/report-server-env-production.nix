{ IOHKaccessKeyId, globals, ... }: with (import ./../lib.nix);

let params = globals.fullMap.report-server;
    accessKeyId = IOHKaccessKeyId;
in
{
  report-server = { resources, ...}: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:${globals.environment}"];

    deployment.route53.accessKeyId = accessKeyId;
    deployment.route53.hostName = "report-server.${(envSpecific globals.environment).dnsSuffix}";
    deployment.ec2.securityGroups = mkForce (map (x: resources.ec2SecurityGroups.${x}) params.sgNames);
    # deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
  };
  # resources.elasticIPs.report-server-ip =
  #   { config, ...}:
  #   let region = config.deployment.ec2.region;
  #   in { inherit region accessKeyId; };
}
