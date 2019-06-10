{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.monitoring-services;
  monitoredNodeOptions = { name, config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
      };
      labels = mkOption {
        type = types.attrs;
        default = {};
        description = "Labels to add in prometheus";
      };
      hasNginx = mkOption {
        type = types.bool;
        default = false;
        description = "if nginx stats should be scraped";
      };
    };
    config = {
      name = mkDefault name;
    };
  };
in {

  options = {
    services.monitoring-services = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable monitoring services.  Metrics collection, analysis
          and alerting is available via prometheus, grafana and alertmanager
          by default.  Logging is available via graylog by default.
          Metrics can be selectively disabled with the metrics option.
          Logging can be selectively disabled with the logging option.
        '';
      };

      enableACME = mkOption {
        type = types.bool;
        default = true;
      };

      enableWireguard = mkOption {
        type = types.bool;
        default = false;
      };

      metrics = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable metrics collection, analysis and alerting via
          prometheus, grafana and alertmanager.
          See also the corresponding metrics exporter option in
          the monitoring-exporters.nix module:
          config.services.monitoring-exporters.metrics
        '';
      };

      logging = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable log collection via graylog and journalbeat.
          This option installs graylog, elasticsearch, mongodb and
          a journalbeat graylog input.
          See also the corresponding journalbeat exporter option in
          the monitoring-exporters.nix module:
          config.services.monitoring-exporters.logging
        '';
      };

      monitoringProject = mkOption {
        type = types.str;
        default = "Cardano";
        description = ''
          The project name using the monitoring services.
        '';
        example = "Cardano";
      };

      monitoringProjectUrl = mkOption {
        type = types.str;
        default = "https://iohk.io/projects/cardano/";
        description = ''
          The URL for the project using the monitoring services.
        '';
        example = "https://iohk.io/projects/cardano/";
      };

      monitoringLargeImgFile = mkOption {
        type = types.path;
        default = ./nginx/cardano-large.svg;
        description = ''
          The file in the modules/nginx directory which contains the html image
          tag and embedded image for the monitoring splash page large image
        '';
        example = "./nginx/cardano-large.svg";
      };

      monitoringSmallImgFile = mkOption {
        type = types.path;
        default = ./nginx/cardano-small.svg;
        description = ''
          The file in the modules/nginx directory which contains the html image
          tag and embedded image for the monitoring splash page small image
        '';
        example = "./nginx/cardano-small.svg";
      };

      monitoringTemplateHtmlFile = mkOption {
        type = types.str;
        default = "monitoring-index-template.html";
        description = ''
          The file in the modules/nginx directory which contains the splash
          page monitoring template html.
        '';
        example = "monitoring-index-template.html";
      };

      extraHeader = mkOption {
        type = types.lines;
        default = "";
        description = ''
          extra html added to the splash page for the monitoring
        '';
        example = "cluster: rc-staging";
      };

      applicationRules = mkOption {
        type = types.listOf types.attrs;
        default = [];
        description = ''
          Application specific alerting rules.
        '';
      };

      applicationDashboards = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Application specific dashboards.
        '';
      };

      grafanaCreds = mkOption {
        type = types.attrs;
        description = ''
          An attribute set containing the username and password of the
          default administative user in grafana.  This attribute set
          is read from a file defined by the makeCreds function
          called in monitoring.nix.  If no such file exists, a
          default user name and password are returned by makeCreds.
          Expected attributes for this set are: "user", "password"
          with string values.
        '';
      };

      grafanaAutoLogin = mkOption {
        type = types.bool;
        default = false;
        description = ''
          auto-login grafana based on oauth2_proxy login
          also auto-registers all new users

          warning: if set at first start, you must set grafanaCreds.user to an email from oauth.emailDomain
          warning: if set after first start, you must authorized a oauth.emailDomain before enabling
          failure to do the above will leave you unable to login with the grafanaCreds (pw login is disabled by this)
          temporarily disable this to make grafanaCreds work, and then do the above
        '';
      };

      graylogCreds = mkOption {
        type = types.attrs;
        description = ''
          An attribute set containing the username, password and
          SHA256 password hash of the default administative user
          in graylog as well as a graylog cluster secret.  This attribute set
          is read from a file defined by the makeCreds function
          called in monitoring.nix.  If no such file exists, a
          default user name and password are returned by makeCreds.
          Expected attributes for this set are: "user", "password",
          "passwordHash" and "clusterSecret", all with with string values.
        '';
      };

      monitoredNodes = mkOption {
        type = types.loaOf (types.submodule monitoredNodeOptions);
        default = {};
        description = ''
          Attribute set of Nodes to be monitored.
        '';
        example = {
          c-a-1 = {
            hasNginx = false;
            labels.role = "core";
          };
        };
      };

      webhost = mkOption {
        type = types.str;
        description = ''
          Public web host used for prometheus, grafana, alertmanager
          and graylog monitoring services.
        '';
        example = "monitoring.lan";
      };

      oauth = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Enable OAuth authication for all monitoring services.
          '';
        };
        provider = mkOption {
          type = types.enum [
            "google"
            "github"
            "azure"
            "gitlab"
            "linkedin"
            "myusa"
          ];
          default = "google";
          description = ''
            OAuth provider.
          '';
        };
        emailDomain = mkOption {
          type = types.str;
          description = ''
            Email domain.
          '';
          example = "iohk.io";
        };
        clientID = mkOption {
          type = types.str;
          description = ''
            The OAuth Client ID.
          '';
          example = "123456.apps.googleusercontent.com";
        };
        clientSecret = mkOption {
          type = types.str;
          description = ''
            The OAuth Client Secret.
          '';
        };
        cookie.secret = mkOption {
          type = types.str;
          description = ''
            The seed string for secure cookies.
          '';
        };
      };

      pagerDuty.serviceKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The pagerDuty service key.
        '';
      };

      deadMansSnitch.pingUrl = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The url that alertmanager should ping regularly to signal it is alive.
        '';
      };

      alertmanager.extraRoutes = mkOption {
        type = types.listOf types.attrs;
        default = [];
        description = "extra routes added to services.prometheus.alertmanager.configuration.route.routes";
      };

      alertmanager.extraReceivers = mkOption {
        type = types.listOf types.attrs;
        default = [];
        description = "extra receivers added to services.prometheus.alertmanager.configuration.receivers";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (lib.mkIf cfg.enableWireguard {
      boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];
      networking.firewall.allowedUDPPorts = [ 51820 ];
      networking.wireguard.interfaces = {
        wg0 = {
          ips = [ "192.168.20.1/24" ];
          listenPort = 51820;
          privateKeyFile = "/etc/wireguard/monitoring.wgprivate";
        };
      };
    })
    (lib.mkIf cfg.oauth.enable (let
      oauthProxyConfig = ''
        auth_request /oauth2/auth;
        error_page 401 = /oauth2/sign_in;

        # pass information via X-User and X-Email headers to backend,
        # requires running with --set-xauthrequest flag
        auth_request_set $user   $upstream_http_x_auth_request_user;
        auth_request_set $email  $upstream_http_x_auth_request_email;
        proxy_set_header X-User  $user;
        proxy_set_header X-Email $email;

        # if you enabled --cookie-refresh, this is needed for it to work with auth_request
        auth_request_set $auth_cookie $upstream_http_set_cookie;
        add_header Set-Cookie $auth_cookie;
      '';
    in {
      services = {
        oauth2_proxy = {
          enable = true;
          inherit (cfg.oauth) clientID clientSecret cookie provider;
          email.domains = [ "${cfg.oauth.emailDomain}" ];
          nginx.virtualHosts = [ "${cfg.webhost}" ];
          setXauthrequest = true;
        };
        nginx.virtualHosts."${cfg.webhost}".locations = {
          "/grafana/".extraConfig = oauthProxyConfig;
          "/prometheus/".extraConfig = oauthProxyConfig;
          "/alertmanager/".extraConfig = oauthProxyConfig;
          "/graylog/".extraConfig = oauthProxyConfig;
        };
      };
    }))
    {
      networking.firewall.allowedTCPPorts = [ 80 ];
      environment.systemPackages = with pkgs; [ goaccess ];

      services = {
        nginx = {
          enable = true;
          commonHttpConfig = ''
            log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                             '"$request" $status $body_bytes_sent '
                             '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
            access_log /var/spool/nginx/logs/access.log x-fwd;
          '';
          virtualHosts = {
            "${cfg.webhost}" = {
              locations = {
                "/" = let
                  monitoringHtml = ''
                    ${cfg.monitoringProject} Monitoring<br>
                    ${if cfg.metrics then ''
                      <span style=font-size:65%><a href=/grafana/ target=_blank class=cardano style="color: #ddc6f2">Grafana</a></span><br>
                      <span style=font-size:65%><a href=/prometheus/ target=_blank class=cardano style="color: #ddc6f2">Prometheus</a></span><br>
                    '' else ''
                      <span style=font-size:65%>Grafana (Disabled)</span><br>
                      <span style=font-size:65%>Prometheus (Disabled)</span><br>
                    ''}
                    ${if config.services.prometheus.alertmanager.enable then ''
                      <span style=font-size:65%><a href=/alertmanager/ target=_blank class=cardano style="color: #ddc6f2">Alertmanager</a></span><br>
                    '' else ''
                      <span style=font-size:65%>Alertmanager (Disabled)</span><br>
                    ''}
                    ${if cfg.logging then ''
                      <span style=font-size:65%><a href=/graylog/ target=_blank class=cardano style="color: #ddc6f2">Graylog</a></span><br>
                    '' else ''
                      <span style=font-size:65%>Graylog (Disabled)<span><br>
                    ''}
                  '';
                  indexFile = pkgs.substituteAll {
                      src = ./nginx + "/${cfg.monitoringTemplateHtmlFile}";
                      inherit (cfg) monitoringProject monitoringProjectUrl extraHeader;
                      inherit monitoringHtml;
                      monitoringSmallImg = ''src="${builtins.baseNameOf cfg.monitoringSmallImgFile}" width="138" height="40"'';
                      monitoringLargeImg = ''src="${builtins.baseNameOf cfg.monitoringLargeImgFile}" width="100"'';
                    };
                  rootDir = pkgs.runCommand "nginx-root-dir" {} ''
                    mkdir $out
                    cd $out
                    cp -v "${cfg.monitoringSmallImgFile}" "${builtins.baseNameOf cfg.monitoringSmallImgFile}"
                    cp -v "${cfg.monitoringLargeImgFile}" "${builtins.baseNameOf cfg.monitoringLargeImgFile}"
                    cp -v ${indexFile} index.html
                  '';
                in {
                  extraConfig = ''
                    etag off;
                    add_header etag "\"${builtins.substring 11 32 rootDir}\"";
                    root ${rootDir};
                  '';
                };
              };
            };
          };
        };
      };
    }
    (lib.mkIf cfg.enableACME {
      networking.firewall.allowedTCPPorts = [ 443 ];
      services.nginx.virtualHosts."${cfg.webhost}" = {
        enableACME = true;
        forceSSL = true;
      };
    })

    (lib.mkIf cfg.grafanaAutoLogin {
      services.grafana.extraOptions = {
        # https://grafana.com/docs/auth/auth-proxy/
        AUTH_PROXY_ENABLED = "true";
        AUTH_PROXY_HEADER_NAME = "X-Email";
        AUTH_PROXY_HEADER_PROPERTY = "email";
        AUTH_PROXY_AUTO_SIGN_UP = "true";
        AUTH_PROXY_WHITELIST = "127.0.0.1, ::1"; # only trust nginx to claim usernames
      };
    })
    (lib.mkIf cfg.metrics {
      services = {
        nginx = {
          enable = true;
          virtualHosts."${cfg.webhost}".locations = {
            "/grafana/".extraConfig = ''
              proxy_pass http://localhost:3000/;
              proxy_set_header Host $host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
            "/prometheus/".extraConfig = ''
              proxy_pass http://localhost:9090/prometheus/;
              proxy_set_header Host $host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
            "/alertmanager/".extraConfig = ''
              proxy_pass http://localhost:9093/;
              proxy_set_header Host $host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
          };
        };
        grafana = {
          enable = true;
          users.allowSignUp = false;
          addr = "";
          domain = "${cfg.webhost}";
          rootUrl = "%(protocol)ss://%(domain)s/grafana/";
          extraOptions = lib.mkIf cfg.oauth.enable {
            AUTH_GOOGLE_ENABLED = "true";
            AUTH_GOOGLE_CLIENT_ID = cfg.oauth.clientID;
            AUTH_GOOGLE_CLIENT_SECRET = cfg.oauth.clientSecret;
          };
          provision = {
            enable = true;
            datasources = [
              {
                type = "prometheus";
                name = "prometheus";
                url = "http://localhost:9090/prometheus";
              }
            ];
            dashboards = [
              {
                name = "generic";
                options.path = ./grafana/generic;
              }] ++ (if (cfg.applicationDashboards != null) then [
              {
                name = "application";
                options.path = cfg.applicationDashboards;
              }] else []);
          };
          security = {
            adminPassword = traceValFn (x:
              if x == "changeme" then ''
                *
                ******************************************************************************************
                WARNING: The grafana default administrative password is "${x}".
                         Please customize this in the static grafana credentials file.

                         If you don't have a grafana static credentials file yet, the following
                         nix-shell script will create one when run from the git clone root directory:

                           nix-shell modules/gen-grafana-creds.nix [--argstr user x] [--argstr password y]

                         If the optional argument strings of user and password are not supplied, a
                         root administrative user will be created with a randomized password.
                ******************************************************************************************
              '' else ''
                Grafana custom administrative password declared'')
              cfg.grafanaCreds.password;
            adminUser = traceValFn (x:
              if x == "changeme" then ''
                *
                ******************************************************************************************
                WARNING: The grafana default administative user name is "${x}".
                         Please customize this in the static grafana credentials file.

                         If you don't have a grafana static credentials file yet, the following
                         nix-shell script will create one when run from the git clone root directory:

                           nix-shell modules/gen-grafana-creds.nix [--argstr user x] [--argstr password y]

                         If the optional argument strings of user and password are not supplied, a
                         root administrative user will be created with a randomized password.
                ******************************************************************************************
              '' else ''
                Grafana custom administrative user name declared'')
              cfg.grafanaCreds.user;
          };
        };
        prometheus.exporters = {
          blackbox = {
            enable = true;
            configFile = pkgs.writeText "blackbox-exporter.yaml" (builtins.toJSON {
              modules = {
                https_2xx = {
                  prober = "http";
                  timeout = "5s";
                  http = {
                    fail_if_not_ssl = true;
                  };
                };
                htts_2xx = {
                  prober = "http";
                  timeout = "5s";
                };
                ssh_banner = {
                  prober = "tcp";
                  timeout = "10s";
                  tcp = {
                    query_response = [ { expect = "^SSH-2.0-"; } ];
                  };
                };
                tcp_v4 = {
                  prober = "tcp";
                  timeout = "5s";
                  tcp = {
                    preferred_ip_protocol = "ip4";
                  };
                };
                tcp_v6 = {
                  prober = "tcp";
                  timeout = "5s";
                  tcp = {
                    preferred_ip_protocol = "ip6";
                  };
                };
                icmp_v4 = {
                  prober = "icmp";
                  timeout = "60s";
                  icmp = {
                    preferred_ip_protocol = "ip4";
                  };
                };
                icmp_v6 = {
                  prober = "icmp";
                  timeout = "5s";
                  icmp = {
                    preferred_ip_protocol = "ip6";
                  };
                };
              };
            });
          };
        };
        prometheus.alertmanager = {
          enable = cfg.pagerDuty.serviceKey != null;
          configuration = {
            route = {
              group_by = [ "alertname" "alias" ];
              group_wait = "30s";
              group_interval = "2m";
              receiver = "team-pager";
              routes = cfg.alertmanager.extraRoutes ++ [
                {
                  match = {
                    severity = "page";
                  };
                  receiver = "team-pager";
                }
              ] ++ (if (cfg.deadMansSnitch.pingUrl != null) then [{
                  match = {
                    alertname = "DeadMansSnitch";
                  };
                  repeat_interval = "5m";
                  receiver = "deadmanssnitch";
                }] else []);
            };
            receivers = cfg.alertmanager.extraReceivers ++ [
              {
                name = "team-pager";
                pagerduty_configs = [
                  {
                    service_key = cfg.pagerDuty.serviceKey;
                  }
                ];
              }
              ] ++ (if (cfg.deadMansSnitch.pingUrl != null) then [
              {
                name = "deadmanssnitch";
                webhook_configs = [{
                  send_resolved = false;
                  url = cfg.deadMansSnitch.pingUrl;
                }];
              }
            ] else []);
          };
        };
        prometheus2 = {
          enable = true;
          webExternalUrl = "https://${cfg.webhost}/prometheus/";
          extraFlags = [
            "--storage.tsdb.retention=8760h"
          ];

          alertmanagerURL = [ "localhost:9093" ];
          rules = [ (builtins.toJSON {
              groups = [
                {
                  name = "alerting-pipeline";
                  rules = [
                    {
                      alert = "DeadMansSnitch";
                      expr = "vector(1)";
                      labels = {
                        severity = "critical";
                      };
                      annotations = {
                        summary = "Alerting DeadMansSnitch.";
                        description = "This is a DeadMansSnitch meant to ensure that the entire Alerting pipeline is functional.";
                      };
                    }
                  ];
                }
                {
                  name = "system";
                  rules = [
                    {
                      alert = "node_down";
                      expr = "up == 0";
                      for = "5m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Node is down.";
                        description = "{{$labels.alias}} has been down for more than 5 minutes.";
                      };
                    }
                    {
                      alert = "node_systemd_service_failed";
                      expr = "node_systemd_unit_state{state=\"failed\"} == 1";
                      for = "4m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Service {{$labels.name}} failed to start.";
                        description = "{{$labels.alias}} failed to (re)start service {{$labels.name}}.";
                      };
                    }
                    {
                      alert = "node_filesystem_full_90percent";
                      expr = "sort(node_filesystem_free_bytes{device!=\"ramfs\"} < node_filesystem_size_bytes{device!=\"ramfs\"} * 0.1) / 1024^3";
                      for = "5m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Filesystem is running out of space soon.";
                        description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} got less than 10% space left on its filesystem.";
                      };
                    }
                    {
                      alert = "node_filesystem_full_in_4h";
                      expr = "predict_linear(node_filesystem_free_bytes{device!=\"ramfs\",device!=\"tmpfs\"}[4h], 4*3600) <= 0";
                      for = "5m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Filesystem is running out of space in 4 hours.";
                        description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} is running out of space of in approx. 4 hours";
                      };
                    }
                    {
                      alert = "node_filedescriptors_full_in_3h";
                      expr = "predict_linear(node_filefd_allocated[1h], 3*3600) >= node_filefd_maximum";
                      for = "20m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}} is running out of available file descriptors in 3 hours.";
                        description = "{{$labels.alias}} is running out of available file descriptors in approx. 3 hours";
                      };
                    }
                    {
                      alert = "node_load1_90percent";
                      expr = "node_load1 / on(alias) count(node_cpu_seconds_total{mode=\"system\"}) by (alias) >= 0.9";
                      for = "1h";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Running on high load.";
                        description = "{{$labels.alias}} is running with > 90% total load for at least 1h.";
                      };
                    }
                    {
                      alert = "node_cpu_util_90percent";
                      expr = "100 - (avg by (alias) (irate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100) >= 90";
                      for = "1h";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: High CPU utilization.";
                        description = "{{$labels.alias}} has total CPU utilization over 90% for at least 1h.";
                      };
                    }
                    {
                      alert = "node_ram_using_99percent";
                      expr = "node_memory_MemFree_bytes + node_memory_Buffers_bytes + node_memory_Cached_bytes < node_memory_MemTotal_bytes * 0.01";
                      for = "30m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Using lots of RAM.";
                        description = "{{$labels.alias}} is using at least 90% of its RAM for at least 30 minutes now.";
                      };
                    }
                    {
                      alert = "node_swap_using_80percent";
                      expr = "node_memory_SwapTotal_bytes - (node_memory_SwapFree_bytes + node_memory_SwapCached_bytes) > node_memory_SwapTotal_bytes * 0.8";
                      for = "10m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Running out of swap soon.";
                        description = "{{$labels.alias}} is using 80% of its swap space for at least 10 minutes now.";
                      };
                    }
                    {
                      alert = "node_time_unsync";
                      expr = "abs(node_timex_offset_seconds) > 0.050 or node_timex_sync_status != 1";
                      for = "1m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: Clock out of sync with NTP";
                        description = "{{$labels.alias}} Local clock offset is too large or out of sync with NTP";
                      };
                    }
                    {
                      alert = "http_high_internal_error_rate";
                      expr = "rate(nginx_vts_server_requests_total{code=\"5xx\"}[5m]) * 50 > on(alias, host) rate(nginx_vts_server_requests_total{code=\"2xx\"}[5m])";
                      for = "15m";
                      labels = {
                        severity = "page";
                      };
                      annotations = {
                        summary = "{{$labels.alias}}: High http internal error (code 5xx) rate";
                        description = "{{$labels.alias}}  number of correctly served requests is less than 50 times the number of requests aborted due to an internal server error";
                      };
                    }
                  ];
                }
                {
                  name = "application";
                  rules = cfg.applicationRules;
                }
              ];
            })];
          scrapeConfigs = [
            {
              job_name = "prometheus";
              scrape_interval = "5s";
              metrics_path = "/prometheus/metrics";
              static_configs = [
                {
                  targets = [
                    "localhost:9090"
                  ];
                  labels = { alias = "prometheus"; };
                }
              ];
            }
            {
              job_name = "node";
              scrape_interval = "10s";
              static_configs = let
                makeNodeConfig = key: value: {
                  targets = [ "${key}:9100" "${key}:9102" ];
                  labels = {
                    alias = key;
                  } // value.labels;
                };
              in mapAttrsToList makeNodeConfig cfg.monitoredNodes;
            }
            {
              job_name = "nginx";
              scrape_interval = "5s";
              metrics_path = "/status/format/prometheus";
              static_configs = let
                makeNodeConfig = key: value: {
                  targets = [ "${key}:9113" ];
                  labels = {
                    alias = key;
                  } // value.labels;
                };
                onlyNginx = n: v: v.hasNginx;
              in mapAttrsToList makeNodeConfig (filterAttrs onlyNginx cfg.monitoredNodes);
            }
          ];
        };
      };
    })

    (lib.mkIf cfg.logging {
      # The following Graylog warning matches a similar Grafana auto-generated warning
      warnings = [ "Graylog passwords will be stored as plaintext in the Nix store!" ];
      networking.firewall.allowedTCPPorts = [ 5044 ];
      systemd.services.graylog = {
        environment = {
          JAVA_HOME = lib.mkForce pkgs.jre_headless; # until fix gets upstreamed
        };
        serviceConfig = {
          TimeoutStartSec = "10m";
          # maybe upstream the post-start?
          ExecStartPost = pkgs.writeScript "graylog-post-start" ''
            #!${pkgs.stdenv.shell}
            export PATH=${pkgs.netcat}/bin:$PATH

            for x in {1..100}; do
              echo loop $x
              nc -z localhost 9000 && break
              echo "waiting 10 sec"
              sleep 10
            done
          '';
        };
      };
      services = {
        nginx = {
          enable = true;
          virtualHosts."${cfg.webhost}".locations = {
            "/graylog/".extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
              proxy_set_header X-Graylog-Server-URL /graylog;
              proxy_set_header X-Forward-Host $host;
              proxy_set_header X-Forwarded-Server $host;

              # Required to partially fix the API Browser, but also breaks the streaming page
              # rewrite ^ $request_uri;
              rewrite ^/graylog/(.*)$ /$1 break;
              # return 400;                                   # https://stackoverflow.com/q/28684300
              proxy_pass http://localhost:9000/;
            '';
          };
        };
        graylog = {
          enable = true;
          nodeIdFile = "/var/lib/graylog/node-id";
          plugins = with pkgs.graylogPlugins; [ auth_sso pagerduty slack ];
          passwordSecret = (
            if cfg.graylogCreds ? clusterSecret then
              cfg.graylogCreds.clusterSecret
            else
              builtins.trace ''
                ***********************************************************************************
                ******
                ******
                ******
                ****** GRAYLOG CLUSTER SECRET NEEDED
                ******
                ******
                ****** REQUIREMENT: To enable a monitoring deployment which includes Graylog,
                ******              a cluster specific pepper secret must be declared.
                ******
                ****** ACTION:      Create a clusterSecret string attribute in the static
                ******              graylog credentials file.
                ******
                ****** COMMAND:     The following example command generates such a string:
                ******
                ******
                ******              tr -cd '[:alnum:]' < /dev/urandom | head -c 96
                ******
                ******
                ****** NOTE:        If you don't have a graylog static credentials file yet,
                ******              the following nix-shell script will create one when run
                ******              from the git clone root directory:
                ******
                ******
                ******     nix-shell modules/gen-graylog-creds.nix [--argstr user x] [--argstr password y]
                ******
                ******
                ******              If the optional argument strings of user and password are
                ******              not supplied, a root administrative user will be created
                ******              with a randomized password.  A SHA256 password hash and a
                ******              randomized cluster secret will also be created.
                ******
                ******
              '' (abort "Graylog cluster secret required")
          );
          rootUsername = traceValFn (x:
            if x == "changeme" then ''
              *
              **********************************************************************
              WARNING: The graylog default administrative user name is "${x}".
                       Please customize this in the static graylog credentials file.
              **********************************************************************
            '' else ''
                Graylog custom administrative user name declared'')
            cfg.graylogCreds.user;
          rootPasswordSha2 = (
            if cfg.graylogCreds ? passwordHash then
              cfg.graylogCreds.passwordHash
            else
              builtins.trace ''
                ***********************************************************************************
                ******
                ******
                ******
                ****** GRAYLOG PASSWORD HASH NEEDED
                ******
                ******
                ****** REQUIREMENT: To enable a monitoring deployment which includes Graylog,
                ******              an administrative user SHA256 password hash created from
                ******              the plaintext password must be provided.
                ******
                ****** ACTION:      Create a passwordHash string attribute in the static
                ******              graylog credentials file by hashing the administrative user's
                ******              plaintext password as input.
                ******
                ****** COMMAND:     The following example command generates such a string, where
                ******              <password> is the plaintext password string of the administrative
                ******              user, also defined in the static graylog credentials file:
                ******
                ******              echo -n <password> | shasum -a 256 | sed -z 's/  -\n//g'
                ******
                ******
                ****** NOTE:        If you don't have a graylog static credentials file yet,
                ******              the following nix-shell script will create one when run
                ******              from the git clone root directory:
                ******
                ******
                ******     nix-shell modules/gen-graylog-creds.nix [--argstr user x] [--argstr password y]
                ******
                ******
                ******              If the optional argument strings of user and password are
                ******              not supplied, a root administrative user will be created
                ******              with a randomized password.  A SHA256 password hash and a
                ******              randomized cluster secret will also be created.
                ******
                ******
              '' (abort "Graylog password hash required")
            );
          elasticsearchHosts = [ "http://localhost:9200" ];
          # Elasticsearch config below is for a single node deployment
          extraConfig = ''
            http_bind_address = 0.0.0.0:9000
            elasticsearch_shards = 1
            elasticsearch_replicas = 0
          '';
        };
        elasticsearch = {
          enable = true;
          package = pkgs.elasticsearch6-oss;
          # Prevent graylog deflector indexing by turning off auto create index option
          extraConf = ''
            action.auto_create_index: false
          '';
        };
        mongodb.enable = true;
      };
      systemd.services.graylog-preload = let
        graylogConfig = ./graylog/graylogConfig.json;
        password = traceValFn (x:
          if x == "changeme" then ''
            *
            ******************************************************************************************
            WARNING: The graylog default administrative password is "${x}".
                     Please customize this in the static graylog credentials file.

                     If you don't have a graylog static credentials file yet, the following
                     nix-shell script will create one when run from the git clone root directory:

                       nix-shell modules/gen-graylog-creds.nix [--argstr user x] [--argstr password y]

                     If the optional argument strings of user and password are not supplied, a
                     root administrative user will be created with a randomized password.
            ******************************************************************************************
          '' else ''
            Graylog custom administrative password declared'')
          cfg.graylogCreds.password;
      in {
        description = "Graylog Content Pack Preload Service";
        wantedBy = [ "multi-user.target" ];
        after = [ "graylog.service elasticsearch.service mongodb.service" ];
        path = with pkgs; [ curl gnugrep jq ];
        serviceConfig = {
          Type = "oneshot";
          User = "${config.services.graylog.user}";
          ExecStartPre = pkgs.writeScript "graylog-preload-prestart" ''
            #!${pkgs.stdenv.shell}
            chmod 0644 /var/lib/graylog/.graylogConfigured* || true
            chown graylog:nogroup /var/lib/graylog/.graylogConfigured* || true
          '';
          ExecStart = let
            graylogPreload = pkgs.substituteAll {
              src = ./graylog/graylogPreload.sh;
              inherit (cfg.graylogCreds) user;
              inherit password;
              isExecutable = true;
            };
          in pkgs.writeScript "graylog-start" ''
            #!${pkgs.stdenv.shell}
            ${graylogPreload} install ${graylogConfig}
          '';
        };
      };
    })
  ]);
}
