with (import ./../lib.nix);

params: environment:
{ pkgs, config, resources, options, ...}:

let
  cfg = config.services.report-server;
in {
  imports = [
    ./common.nix
    ./amazon-base.nix
    ./network-wide.nix
  ];

  options = {
    services.report-server = {
      logsdir = mkOption {
        type = types.path;
        default = "/var/lib/report-server";
      };
      port = mkOption {
        type = types.int;
        default = 8080;
      };
      executable = mkOption {
        type = types.package;
        default = (import ./../default.nix {}).cardano-report-server-static;
      };
      zendesk = {
        email = mkOption {
          type = types.str;
          # default = "";
          default = if environment == "Staging"
            then "report-server-zendesk+staging@iohk.io"
            else (if environment == "Production"
                  then "report-server-zendesk+prod@iohk.io"
                  else "");
          example = "agent@email.com";
          description = ''
            The e-mail associated with the Zendesk account.
          '';
        };
        tokenFile = mkOption {
          type = types.nullOr types.path;
          # default = null;
          default = if environment == "Staging" || environment == "Production"
            then "/run/keys/zendesk-token"
            else null;
          example = "/run/keys/zendesk-token";
          description = ''
            An access token for Zendesk.
          '';
        };
      };
    };
  };

  config = {

    # TODO: remove
    global = {
      organisation             = params.org;
      dnsHostname              = mkForce "report-server";
    };

    deployment.ec2.region         = mkForce params.region;
    deployment.ec2.accessKeyId    = params.accessKeyId;
    deployment.ec2.keyPair        = resources.ec2KeyPairs.${params.keyPairName};
    deployment.ec2.securityGroups =
      let sgNames = [ "allow-to-report-server-${config.deployment.ec2.region}" ];
      in map (resolveSGName resources)
         (if config.global.omitDetailedSecurityGroups
          then [ "allow-all-${params.region}-${params.org}" ]
          else sgNames);

    deployment.ec2.ebsInitialRootDiskSize = 200;

    deployment.keys.zendesk-token = {
      keyFile = ./. + "/../static/zendesk-token.secret";
      user = "report-server";
    };

    networking.firewall.allowedTCPPorts = [
      cfg.port
    ];

    users = {
      users.report-server = {
        group = "report-server";
        home = config.services.report-server.logsdir;
        createHome = true;
        extraGroups = [ "keys" ];
      };
      groups.report-server = {};
    };

    systemd.services.report-server = {
      description   = "Cardano report server";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig.RequiresMountsFor = cfg.logsdir;
      serviceConfig = {
        User = "report-server";
        Group = "report-server";
      };
      script = let
        zdEmail = if cfg.zendesk.email != "" then "--zd-email \"${cfg.zendesk.email}\"" else "";
        # fixme: report-server should not accept token as command-line argument
        zdToken = if cfg.zendesk.tokenFile != null then "--zd-token `head -1 ${cfg.zendesk.tokenFile}`" else "";
      in ''
        exec ${cfg.executable}/bin/cardano-report-server \
            -p ${toString cfg.port} \
            ${zdEmail} ${zdToken} \
            --logsdir ${cfg.logsdir}
      '';
    };

    assertions = [
      { assertion =
          (cfg.zendesk.email != "" && cfg.zendesk.tokenFile != null) ||
          (cfg.zendesk.email == "" && cfg.zendesk.tokenFile == null);
        message = ''
          Both `services.report-server.zendesk.email'
          and `services.report-server.zendesk.tokenFile' must be defined.
        '';
      }
    ];
  };
}
