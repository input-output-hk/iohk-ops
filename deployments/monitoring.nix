{ globals, ... }: with (import ./../lib.nix);
let
  nodeMap = { inherit (globals.fullMap) monitoring; };
  monitoring = nodeMap.monitoring;
in

{
  monitoring = { config, lib, pkgs, resources, nodes, ... }: 
  let
    hostList = lib.mapAttrsToList 
      (nodeName: node: {
        name = "${nodeName}.${node.config.deployment.name}"; 
        ip = node.config.networking.publicIPv4;
        withNginx = node.config.services.nginx.enable;
      }) nodes;
    hostName = "monitoring.${config.global.dnsDomainname}";
  in
  {
    imports = [
      ../modules/common.nix
      ../modules/amazon-base.nix
      ../modules/network-wide.nix
      ../modules/monitoring-services.nix
    ];

    global = {
      organisation = monitoring.org;
      dnsHostname  = mkForce "monitoring";
    };

    networking.extraHosts = ''
      ${concatStringsSep "\n" (map (host: "${toString host.ip} ${host.name}") hostList)}
    '';
 
    services.monitoring-services = {
      enable = true;
      oauth = {
        enable = true;
        emailDomain = "iohk.io";
      } // (import ../static/oauth.nix);
      monitoredNodes = map (h: h.name) (lib.filter (h: !h.withNginx) hostList);
      nginxMonitoredNodes = map (h: h.name) (lib.filter (h: h.withNginx) hostList);
      webhost = hostName;
      pagerDuty = import ../static/pager-duty.nix;
      deadMansSnitch = import ../static/dead-mans-snitch.nix;
    };

    deployment.ec2.region         = mkForce monitoring.region;
    deployment.ec2.accessKeyId    = monitoring.accessKeyId;
    deployment.ec2.keyPair        = resources.ec2KeyPairs.${monitoring.keyPairName};
    deployment.ec2.securityGroups = [
      resources.ec2SecurityGroups."allow-to-monitoring-${config.deployment.ec2.region}"
    ];
  };
      
  resources.elasticIPs = nodesElasticIPs nodeMap;
}

