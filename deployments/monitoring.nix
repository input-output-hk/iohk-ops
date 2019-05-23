{ globals, ... }:

with import ../lib.nix;
let
  nodeMap = { inherit (globals.fullMap) monitoring; };
  monitoring = nodeMap.monitoring;
in

{
  # configure all machines in the cluster so they can find graylog
  defaults = { config, lib, ... }: {
    _file = ./monitoring.nix;
    services.monitoring-exporters = {
      # TODO, `monitoring-ip` will be wrong if monitoring isnt using an elastic ip by that name
      graylogHost = "monitoring-ip:5044";
      #graylogHost = "${config.deployment.arguments.globals.monitoringNV.name}-ip:5044";
      ownIp = let
        ip = config.networking.publicIPv4;
      in if ip == null then "0.0.0.0" else ip;
    };
  };
  monitoring = { config, lib, pkgs, resources, nodes, ... }:
  let
    # a list of { name=; ip=; withNginx=; } for every node in the deployment
    hostList = lib.mapAttrsToList
      (nodeName: node: {
        name = "${nodeName}.${node.config.deployment.name}";
        ip = node.config.services.monitoring-exporters.ownIp;
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

    # add everything from hostList to /etc/hosts
    # if a machine is using wireguard, the `services.monitoring-exporters.ownIp` will be the WG ip, and this will point to that
    networking.extraHosts = ''
      ${concatStringsSep "\n" (map (host: "${toString host.ip} ${host.name}") hostList)}
    '';

    services.monitoring-services = {
      enable = true;
      metrics = true;
      logging = true;
      monitoringProject = "Cardano";
      monitoringProjectUrl = "https://iohk.io/projects/cardano";
      oauth = {
        enable = true;
        emailDomain = "iohk.io";
      } // (import ../static/oauth.nix);


      # NOTE: The Grafana user and password settings only take effect on the initial deployment.
      grafanaCreds = makeCreds "grafana" { user = "changeme"; password = "changeme"; };
      graylogCreds = makeCreds "graylog" { user = "changeme"; password = "changeme"; };

      monitoredNodes = map (h: h.name) (lib.filter (h: !h.withNginx) hostList);
      nginxMonitoredNodes = map (h: h.name) (lib.filter (h: h.withNginx) hostList);
      webhost = hostName;
      pagerDuty = import ../static/pager-duty.nix;
      deadMansSnitch = import ../static/dead-mans-snitch.nix;
    };
  };
}
