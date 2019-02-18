{ environment ? "staging", ... }:

{
  require = [ (./adapay-aws- + "${environment}.nix") ];
  network.description = "Adapay";

  defaults = { ... }: {
    imports = [
      ../modules/common.nix
      ../modules/datadog.nix
      ../modules/papertrail.nix
      ../modules/cardano-importer.nix
      ../modules/adapay.nix
      ../modules/icarus-backend.nix
    ];
    services = {
      dd-agent.tags = [
        "env:${environment}"
        "role:adapay"
      ];
      prometheus.exporters.node = {
        enable = true;
        enabledCollectors = [
          "systemd"
          "tcpstat"
          "conntrack"
          "diskstats"
          "entropy"
          "filefd"
          "filesystem"
          "loadavg"
          "meminfo"
          "netdev"
          "netstat"
          "stat"
          "time"
          "vmstat"
          "logind"
          "interrupts"
          "ksmd"
        ];
      };
    };
    networking.firewall.allowedTCPPorts = [ 9100 ];
    users.users = if builtins.pathExists ../static/extra-users.nix then import ../static/extra-users.nix else { };
  };
  nginx = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 80 443 9113 ];
    services = {
      nginx = {
        enable = true;
        package = with pkgs; nginxStable.override {
          modules = [ nginxModules.rtmp nginxModules.dav nginxModules.moreheaders nginxModules.vts ];
        };
        appendHttpConfig = ''
          vhost_traffic_status_zone;
        '';
        virtualHosts = {
          "${environment}.adapay.iohk.io" = {
            enableACME = true;
            forceSSL = true;
            locations."/".extraConfig = ''
              proxy_pass http://adapay:8081;
              proxy_set_header Host $http_host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
          };
          "nginx" = {
            listen = [{ addr = "0.0.0.0"; port = 9113; }];
            locations."/status".extraConfig = ''
              vhost_traffic_status_display;
              vhost_traffic_status_display_format html;
            '';

          };
        };
      };
    };
  };
  importer = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 8200 ];
    environment.systemPackages = with pkgs; [
      postgresql
    ];
    services = {
      cardano-importer = let
        pgConfig = import (../static/importer-pgconfig- + "${environment}.nix");
      in {
        inherit environment;
        inherit (pgConfig) pguser pgdb pghost;
        enable = true;
        pgpwFile = "/run/keys/importer-pg-pw";
      };
    };
    deployment.keys = {
      importer-pg-pw = {
        keyFile = ../static/cardano-importer-pg-pw.secret;
        user = "cardano";
      };

    };
  };
  adapay = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 8081 ];
    environment.systemPackages = with pkgs; [
      postgresql
    ];
    services = {
      icarus-backend = {
        inherit environment;
        enable = true;
      };
      adapay = {
        inherit environment;
        enable = true;
      };
    };
    deployment.keys = {
      "icarus-backend-${environment}.js" = {
        keyFile = ../static/icarus-backend + "-${environment}.js";
        user = "icarus-backend";
      };
      "adapay-${environment}.js" = {
        keyFile = ../static/adapay + "-${environment}.js";
        user = "adapay";
      };
    };
  };
  monitoring = { config, pkgs, resources, ... }: {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    services = let
      oauthCreds = import ../static/adapay-oauth.nix;
    in {
      nginx = {
        enable = true;
        package = with pkgs; nginxStable.override {
          modules = [ nginxModules.rtmp nginxModules.dav nginxModules.moreheaders nginxModules.vts ];
        };
        appendHttpConfig = ''
          vhost_traffic_status_zone;
        '';
        virtualHosts = {
          "monitoring.${environment}.adapay.iohk.io" = {
            enableACME = true;
            forceSSL = true;
            locations."/grafana/".extraConfig = ''
              proxy_pass http://localhost:3000/;
              proxy_set_header Host $http_host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
            locations."/prometheus/".extraConfig = ''
              proxy_pass http://monitoring:9090/;
              proxy_set_header Host $http_host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
              sub_filter_types text/html;
              sub_filter_once off;
              sub_filter '="/' '="/prometheus/';
              sub_filter '="/static/' '="/static/prometheus/';
            '';
            locations."/alertmanager/".extraConfig = ''
              proxy_pass http://monitoring:9093/;
              proxy_set_header Host $http_host;
              proxy_set_header REMOTE_ADDR $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto https;
            '';
          };
          "monitoring" = {
            listen = [{ addr = "0.0.0.0"; port = 9113; }];
            locations."/status".extraConfig = ''
              vhost_traffic_status_display;
              vhost_traffic_status_display_format html;
            '';

          };
        };
      };
      oauth2_proxy = {
        enable = true;
        inherit (oauthCreds) clientID clientSecret;
        provider = "google";
        email.domains = [ "iohk.io" ];
        nginx.virtualHosts = [ "monitoring.${environment}.adapay.iohk.io" ];
      };
      grafana = {
        enable = true;
        users.allowSignUp = true;
        addr = "";
        domain = "monitoring.${environment}.adapay.iohk.io";
        rootUrl = "%(protocol)ss://%(domain)s/grafana/";
        extraOptions = {
          AUTH_GOOGLE_ENABLED = "true";
          AUTH_GOOGLE_CLIENT_ID = oauthCreds.clientID;
          AUTH_GOOGLE_CLIENT_SECRET = oauthCreds.clientSecret;
        };
      };
      prometheus = {
        enable = true;
        extraFlags = [
          "-storage.local.retention 8760h"
          "-storage.local.series-file-shrink-ratio 0.3"
          "-storage.local.memory-chunks 2097152"
          "-storage.local.max-chunks-to-persist 1048576"
          "-storage.local.index-cache-size.fingerprint-to-metric 2097152"
          "-storage.local.index-cache-size.fingerprint-to-timerange 1048576"
          "-storage.local.index-cache-size.label-name-to-label-values 2097152"
          "-storage.local.index-cache-size.label-pair-to-fingerprints 41943040"
        ];
        exporters = {
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
        alertmanagerURL = [ "http://monitoring:9093" ];
        rules = [
          ''
            ALERT node_down
            IF up == 0
            FOR 5m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: Node is down.",
              description = "{{$labels.alias}} has been down for more than 5 minutes."
            }
            ALERT node_systemd_service_failed
            IF node_systemd_unit_state{state="failed"} == 1
            FOR 4m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: Service {{$labels.name}} failed to start.",
              description = "{{$labels.alias}} failed to (re)start service {{$labels.name}}."
            }
            ALERT node_filesystem_full_90percent
            IF sort(node_filesystem_free{device!="ramfs"} < node_filesystem_size{device!="ramfs"} * 0.1) / 1024^3
            FOR 5m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: Filesystem is running out of space soon.",
              description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} got less than 10% space left on its filesystem."
            }
            ALERT node_filesystem_full_in_4h
            IF predict_linear(node_filesystem_free{device!="ramfs"}[1h], 4*3600) <= 0
            FOR 5m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: Filesystem is running out of space in 4 hours.",
              description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} is running out of space of in approx. 4 hours"
            }
            ALERT node_filedescriptors_full_in_3h
            IF predict_linear(node_filefd_allocated[1h], 3*3600) >= node_filefd_maximum
            FOR 20m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}} is running out of available file descriptors in 3 hours.",
              description = "{{$labels.alias}} is running out of available file descriptors in approx. 3 hours"
            }
            ALERT node_load1_90percent
            IF node_load1 / on(alias) count(node_cpu{mode="system"}) by (alias) >= 0.9
            FOR 1h
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: Running on high load.",
              description = "{{$labels.alias}} is running with > 90% total load for at least 1h."
            }
            ALERT node_cpu_util_90percent
            IF 100 - (avg by (alias) (irate(node_cpu{mode="idle"}[5m])) * 100) >= 90
            FOR 1h
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary = "{{$labels.alias}}: High CPU utilization.",
              description = "{{$labels.alias}} has total CPU utilization over 90% for at least 1h."
            }
            ALERT node_ram_using_99percent
            IF node_memory_MemFree + node_memory_Buffers + node_memory_Cached < node_memory_MemTotal * 0.01
            FOR 30m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary="{{$labels.alias}}: Using lots of RAM.",
              description="{{$labels.alias}} is using at least 90% of its RAM for at least 30 minutes now.",
            }
            ALERT node_swap_using_80percent
            IF node_memory_SwapTotal - (node_memory_SwapFree + node_memory_SwapCached) > node_memory_SwapTotal * 0.8
            FOR 10m
            LABELS {
              severity="page"
            }
            ANNOTATIONS {
              summary="{{$labels.alias}}: Running out of swap soon.",
              description="{{$labels.alias}} is using 80% of its swap space for at least 10 minutes now."
            }
          ''
        ];
        scrapeConfigs = [
          {
            job_name = "prometheus";
            scrape_interval = "5s";
            static_configs = [
              {
                targets = [
                  "localhost:9090"
                ];
              }
            ];
          }
          {
            job_name = "node";
            scrape_interval = "10s";
            static_configs = let
              makeNodeConfig = name: {
                targets = [ "${name}:9100" ];
                labels = { alias = name; };
              };
            in map makeNodeConfig [
              "importer"
              "adapay"
              "nginx"
              "monitoring"
            ];
          }
          {
            job_name = "nginx";
            scrape_interval = "5s";
            metrics_path = "/status/format/prometheus";
            static_configs = [
              {
                targets = [
                  "nginx:9113"
                  "monitoring:9113"
                ];
              }
            ];
          }
        ];
        alertmanager = {
          enable = if environment == "staging" then true else false;
          configuration = {
            route = {
              group_by = [ "alertname" "alias" ];
              group_wait = "30s";
              group_interval = "2m";
              receiver = "team-pager";
              routes = [
                {
                  match = {
                    severity = "page";
                  };
                  receiver = "team-pager";
                }
              ];
            };
            receivers = [
              {
                name = "team-pager";
                pagerduty_configs = [
                  {
                    service_key = builtins.readFile ../static/pagerduty-key.secret;
                  }
                ];
              }
            ];
          };
        };
      };
    };
  };
}
