with (import ./../lib.nix);

{
  report-server = { resources, ...}: {
    imports = [
      ./../modules/datadog.nix
      ./../modules/papertrail.nix
    ];

    services.dd-agent.tags = ["env:production"];

    deployment.ec2.elasticIPv4 = resources.elasticIPs.report-server-ip;
  };
  resources = {
    elasticIPs = {
      report-server-ip = { inherit region accessKeyId; };
    };
  };
}
