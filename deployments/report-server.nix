{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) report-server; }; in

flip mapAttrs nodeMap
(name: import ./../modules/report-server.nix
       globals
       [])
//
{
  resources.elasticIPs = nodesElasticIPs nodeMap;

  deployment.ec2.securityGroups = map (resolveSGName resources) [
    "allow-to-report-server-${config.deployment.ec2.region}"
  ];
}
