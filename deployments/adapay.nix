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
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    services = {
      nginx = {
        enable = true;
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
            locations."/grafana/".extraConfig = ''
              proxy_pass http://monitoring:3000/;
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
    networking.firewall.allowedTCPPorts = [ 3000 ];
    services = {
      grafana = {
        enable = true;
        users.allowSignUp = true;
        domain = "${environment}.adapay.iohk.io";
        rootUrl = "%(protocol)ss://%(domain)s/grafana/";
        extraOptions = {
          AUTH_GOOGLE_ENABLED = "true";
          AUTH_GOOGLE_CLIENT_ID = "778964826061-5v0m922g1qcbc1mdtpaf8ffevlso2v7p.apps.googleusercontent.com";
          AUTH_GOOGLE_CLIENT_SECRET = builtins.readFile ../static/google_oauth_adapay_grafana.secret;
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
        alertmanagerURL = [ "http://${environment}.adapay.iohk.io:9093" ];
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
        ];
        alertmanager = {
          enable = false;
        };
      };
    };
  };
}
