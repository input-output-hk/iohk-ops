{ globals, ... }: with (import ./../lib.nix);

let params = globals.report-server;
in
{
  report-server = { resources, ...}: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
      ./../modules/common.nix
      (import ./../modules/amazon-base.nix globals params)
    ];

    services.dd-agent.tags = ["env:${globals.environment}"];

    deployment.ec2.accessKeyId = params.accessKeyId;
    deployment.ec2.securityGroups = map (sgName: resources.ec2SecurityGroups.${sgName}) params.sgNames;

    deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
    deployment.route53.accessKeyId = params.accessKeyId;
    deployment.route53.hostName = "report-server.${(envSpecific globals.environment).dnsSuffix}";
  };
  resources.elasticIPs.report-server-ip = nodeElasticIP params;
}
