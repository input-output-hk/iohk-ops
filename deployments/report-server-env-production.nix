{ IOHKaccessKeyId, environment, ... }:

with (import ./../lib.nix);
{
  report-server = { resources, ...}: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:${environment}"];

    deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
    deployment.route53.accessKeyId = IOHKaccessKeyId;
    deployment.route53.hostName = "report-server.${(envSpecific environment).dnsSuffix}";
  };
  resources = {
    elasticIPs = {
      report-server-ip = { inherit region IOHKaccessKeyId; };
    };
  };
}
