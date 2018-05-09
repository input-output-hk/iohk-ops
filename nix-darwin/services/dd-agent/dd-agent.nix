{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.dd-agent;

  dataDir = "/var/log/datadog";
  ddConf = pkgs.writeText "datadog.conf" ''
    [Main]
    dd_url: https://app.datadoghq.com
    skip_ssl_validation: no
    api_key: ${cfg.api_key}
    ${optionalString (cfg.hostname != null) "hostname: ${cfg.hostname}"}

    collector_log_file: /var/log/datadog/collector.log
    forwarder_log_file: /var/log/datadog/forwarder.log
    dogstatsd_log_file: /var/log/datadog/dogstatsd.log
    pup_log_file:       /var/log/datadog/pup.log

    # proxy_host: my-proxy.com
    # proxy_port: 3128
    # proxy_user: user
    # proxy_password: password

    # tags: mytag0, mytag1
    ${optionalString (cfg.tags != null ) "tags: ${concatStringsSep ", " cfg.tags }"}

    # collect_ec2_tags: no
    # recent_point_threshold: 30
    # use_mount: no
    # listen_port: 17123
    # graphite_listen_port: 17124
    # non_local_traffic: no
    # use_curl_http_client: False
    # bind_host: localhost

    # use_pup: no
    # pup_port: 17125
    # pup_interface: localhost
    # pup_url: http://localhost:17125

    # dogstatsd_port : 8125
    # dogstatsd_interval : 10
    # dogstatsd_normalize : yes
    # statsd_forward_host: address_of_own_statsd_server
    # statsd_forward_port: 8125

    # device_blacklist_re: .*\/dev\/mapper\/lxc-box.*

    # ganglia_host: localhost
    # ganglia_port: 8651
  '';

  diskConfig = pkgs.writeText "disk.yaml" ''
    init_config:

    instances:
      - use_mount: no
        excluded_filesystems:
          - devfs
  '';

  networkConfig = pkgs.writeText "network.yaml" ''
    init_config:

    instances:
      # Network check only supports one configured instance
      - collect_connection_state: false
        excluded_interfaces:
          - lo
          - lo0
  '';

  postgresqlConfig = pkgs.writeText "postgres.yaml" cfg.postgresqlConfig;
  nginxConfig = pkgs.writeText "nginx.yaml" cfg.nginxConfig;
  mongoConfig = pkgs.writeText "mongo.yaml" cfg.mongoConfig;
  jmxConfig = pkgs.writeText "jmx.yaml" cfg.jmxConfig;
  processConfig = pkgs.writeText "process.yaml" cfg.processConfig;

  etcfiles =
    let
      defaultConfd = import ./dd-agent-defaults.nix;
    in (map (f: { source = "${cfg.package}/agent/conf.d-system/${f}";
                  target = "dd-agent/conf.d/${f}";
                }) defaultConfd) ++ [
      { source = ddConf;
        target = "dd-agent/datadog.conf";
      }
      { source = diskConfig;
        target = "dd-agent/conf.d/disk.yaml";
      }
      { source = networkConfig;
        target = "dd-agent/conf.d/network.yaml";
      } ] ++
    (optional (cfg.postgresqlConfig != null)
      { source = postgresqlConfig;
        target = "dd-agent/conf.d/postgres.yaml";
      }) ++
    (optional (cfg.nginxConfig != null)
      { source = nginxConfig;
        target = "dd-agent/conf.d/nginx.yaml";
      }) ++
    (optional (cfg.mongoConfig != null)
      { source = mongoConfig;
        target = "dd-agent/conf.d/mongo.yaml";
      }) ++
    (optional (cfg.processConfig != null)
      { source = processConfig;
        target = "dd-agent/conf.d/process.yaml";
      }) ++
    (optional (cfg.jmxConfig != null)
      { source = jmxConfig;
        target = "dd-agent/conf.d/jmx.yaml";
      });

in {
  options.services.dd-agent = {
    enable = mkOption {
      description = "Whether to enable the dd-agent monitoring service";
      default = false;
      type = types.bool;
    };

    package = mkOption {
      default = pkgs.dd-agent;
      defaultText = "pkgs.dd-agent";
      description = "Which dd-agent derivation to use";
      type = types.package;
    };

    api_key = mkOption {
      description = "The Datadog API key to associate the agent with your account";
      example = "ae0aa6a8f08efa988ba0a17578f009ab";
      type = types.str;
      default = "";
    };

    apiKeyFile = mkOption {
      description = ''
        Path to a file containing the Datadog API key to associate the agent with your account.
      '';
      example = "/run/keys/datadog_api_key";
      type = types.nullOr types.path;
      default = null;
    };

    tags = mkOption {
      description = "The tags to mark this Datadog agent";
      example = [ "test" "service" ];
      default = null;
      type = types.nullOr (types.listOf types.str);
    };

    hostname = mkOption {
      description = "The hostname to show in the Datadog dashboard (optional)";
      default = null;
      example = "mymachine.mydomain";
      type = types.uniq (types.nullOr types.string);
    };

    postgresqlConfig = mkOption {
      description = "Datadog PostgreSQL integration configuration";
      default = null;
      type = types.uniq (types.nullOr types.string);
    };

    nginxConfig = mkOption {
      description = "Datadog nginx integration configuration";
      default = null;
      type = types.uniq (types.nullOr types.string);
    };

    mongoConfig = mkOption {
      description = "MongoDB integration configuration";
      default = null;
      type = types.uniq (types.nullOr types.string);
    };

    jmxConfig = mkOption {
      description = "JMX integration configuration";
      default = null;
      type = types.uniq (types.nullOr types.string);
    };

    processConfig = mkOption {
      description = ''
        Process integration configuration

        See http://docs.datadoghq.com/integrations/process/
      '';
      default = null;
      type = types.uniq (types.nullOr types.string);
    };

  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package pkgs.darwin.system_cmds ];

    users.groups.datadog = {
      description = "Datadog Agent Group";
      members = [ "datadog" ];
    };
    users.users.datadog = {
      description = "Datadog Agent User";
      home = dataDir;
    };

    launchd.daemons.dd-agent = {
      path = [ cfg.package pkgs.python pkgs.coreutils
        pkgs.darwin.system_cmds "/usr/bin" "/usr/sbin" "/bin" ];
      script = ''
        set -e
        DD_API_KEY=$(head -n1 ${cfg.apiKeyFile})
        ${pkgs.gnused}/bin/sed -e "s/api_key:.*/api_key: $DD_API_KEY/" ${ddConf} > /opt/datadog-agent/etc/datadog.conf
        exec ${cfg.package}/bin/dd-agent foreground
      '';
      serviceConfig = {
        RunAtLoad = true;
        WorkingDirectory = dataDir;
        UserName = "datadog";
        GroupName = "datadog";
        StandardErrorPath = "${dataDir}/dd-agent.log";
        StandardOutPath = "${dataDir}/dd-agent.log";
        KeepAlive = true;
      };
    };

    launchd.daemons.dogstatsd = {
      path = [ cfg.package pkgs.python pkgs.darwin.system_cmds ];
      command = "${cfg.package}/bin/dogstatsd";
      serviceConfig = {
        RunAtLoad = true;
        WorkingDirectory = dataDir;
        UserName = "datadog";
        GroupName = "datadog";
        StandardErrorPath = "${dataDir}/dogstatsd.log";
        StandardOutPath = "${dataDir}/dogstatsd.log";
        KeepAlive = true;
      };
    };

    launchd.daemons.dd-jmxfetch = lib.mkIf (cfg.jmxConfig != null) {
      path = [ cfg.package pkgs.python pkgs.darwin.system_cmds pkgs.jdk ];
      command = "${cfg.package}/bin/dd-jmxfetch";
      serviceConfig = {
        RunAtLoad = true;
        WorkingDirectory = dataDir;
        UserName = "datadog";
        GroupName = "datadog";
        KeepAlive = true;
      };
    };

    environment.etc = etcfiles;

    system.activationScripts.postActivation.text = ''
      mkdir -p /opt/datadog-agent/etc
      chown datadog:datadog /opt/datadog-agent/etc
      chmod 770 /opt/datadog-agent/etc
      ln -sf /etc/dd-agent/conf.d /opt/datadog-agent/etc
    '';

    warnings = optional (cfg.api_key != "")
      ''config.services.dd-agent.api_key will be stored as plaintext
        in the Nix store. Use apiKeyFile instead.'';
    assertions = lib.singleton
      { assertion = cfg.api_key != "" || cfg.apiKeyFile != null;
        message = "config.services.dd-agent.apiKeyFile is not set";
      };

    # Create apiKeyFile default when api_key is configured.
    services.dd-agent.apiKeyFile =
      (mkDefault (toString (pkgs.writeTextFile {
        name = "datadog-agent-api-key";
        text = cfg.api_key;
      })));
  };
}
