with (import ./../lib.nix);

globals: imports: params:
{ pkgs, nodes, config, resources, options, ...}:
let
  report-server-drv = (import ./../default.nix {}).cardano-report-server-static;
in {
  imports = [
    ./global-options.nix
    ./global-config.nix
    ./common.nix
    ./amazon-base.nix
  ];

  global.organisation = params.org;
  global.dnsHostname = mkForce "report-server";

  deployment.ec2.region         = mkForce params.region;
  deployment.ec2.accessKeyId    = params.accessKeyId;
  deployment.ec2.keyPair        = resources.ec2KeyPairs.${params.keyPairName};
  deployment.ec2.securityGroups = map (resolveSGName resources) [
    "allow-to-report-server-${config.deployment.ec2.region}"
  ];

  networking.firewall.allowedTCPPorts = [
    params.port
  ];

  users = {
    users.report-server = {
      group = "report-server";
      home = config.services.report-server.logsdir;
      createHome = true;
    };
    groups.report-server = {};
  };

  systemd.services.report-server = {
    description   = "Cardano report server";
    after         = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "report-server";
      Group = "report-server";
      ExecStart = ''
        ${report-server-drv}/bin/cardano-report-server -p ${toString params.port} --logsdir ${config.services.report-server.logsdir}
      '';
    };
  };
}
  
