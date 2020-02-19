{ ... }:

with import ../lib.nix;

let
  lib = (import <nixpkgs> {}).lib;
  mkHydra = hostname: { config, pkgs, resources, ... }: { };
  mkUplink = mkMkUplink {
    central = "192.168.20.1";
    subnet = "192.168.20";
    # TODO, `monitoring-ip` will be wrong if monitoring isnt using an elastic ip by that name
    endpoint = "monitoring-ip:51820";
  };
  org = "IOHK";
  region = "eu-central-1";
  genPeer = n: path: {
    allowedIPs = [ "192.168.20.${toString n}/32" ];
    publicKey = lib.strings.removeSuffix "\n" (builtins.readFile path);
    persistentKeepalive = 30;
  };
in {
  network.description = "iohk-infra-prod";
  require = [
    ./hydra-master-wireguard.nix
    ./mac-base.nix
  ];

  defaults = {
    _file = ./infrastructure-env-production.nix;
    # TODO imports = [ ../modules/network-wide.nix ];

    services.monitoring-exporters = {
      papertrail.enable = true;
    };
  };

  hydra        = mkHydra "hydra";
  mantis-hydra = mkHydra "mantis-hydra";

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/deployer.nix
    ];

    deployment = {
      keys = {
        tarsnap = {
          keyFile = ../static/tarsnap-cardano-deployer.secret;
          destDir = "/var/lib/keys";
        };
        "cardano-deployer.wgprivate" = {
          destDir = "/etc/wireguard";
          keyFile = ../static/cardano-deployer.wgprivate;
        };
      };
      ec2.securityGroups = [
        resources.ec2SecurityGroups."allow-wireguard-in-${region}-${org}"
      ];
    };

    networking = {
      firewall.allowedUDPPorts = [ 51820 ];
      wireguard.interfaces.wg0 = {
        ips = [ "192.168.20.3/32" ];
        listenPort = 51820;
        privateKeyFile = "/etc/wireguard/cardano-deployer.wgprivate";
        peers = [
          (genPeer 20 ../static/sarov.wgpublic)
          (genPeer 21 ../static/mac-mini-1.wgpublic)
          (genPeer 22 ../static/mac-mini-2.wgpublic)
        ];
      };
    };

    users = {
      users.live-production = {
        description     = "cardano live-production";
        group           = "live-production";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devOpsKeys;
      };
      groups.live-production = {};
      users.staging = {
        description     = "cardano staging";
        group           = "staging";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.staging = {};
    };

    services.tarsnap = {
      enable = true;
      keyfile = "/var/lib/keys/tarsnap";
      archives.cardano-deployer = {
        directories = [
          "/home/live-production/.aws"
          "/home/live-production/.nixops"
          "/etc/"
        ];
      };
    };

  };

  bors-ng = { config, pkgs, resources, ... }: let
    hostName = "bors-ng.aws.iohkdev.io";
    keysDir = "/var/lib/keys";
  in {

    services.bors-ng = {
      publicHost = hostName;
      secretKeyBaseFile = "${keysDir}/bors-ng-secret-key-base";
      github = {
        clientID = "Iv1.17382ed95b58d1a8";
        clientSecretFile = "${keysDir}/bors-ng-github-client-secret";
        integrationID = 17473;
        integrationPEMFile = "${keysDir}/bors-ng-github-integration.pem";
        webhookSecretFile = "${keysDir}/bors-ng-github-webhook-secret";
      };
    };
    systemd.services.bors-ng = {
      after = [ "keys.target" ];
      requires = [ "keys.target" ];
    };
    users.users.bors-ng.extraGroups = [ "keys" ];

    deployment.keys = {
      bors-ng-secret-key-base = {
        keyFile = ../static/bors-ng-secret-key-base;
        destDir = "/var/lib/keys";
        user = "bors-ng";
      };
      bors-ng-github-client-secret = {
        keyFile = ../static/bors-ng-github-client-secret;
        destDir = "/var/lib/keys";
        user = "bors-ng";
      };
      "bors-ng-github-integration.pem" = {
        keyFile = ../static/bors-ng-github-integration.pem;
        destDir = "/var/lib/keys";
        user = "bors-ng";
      };
      bors-ng-github-webhook-secret = {
        keyFile = ../static/bors-ng-github-webhook-secret;
        destDir = "/var/lib/keys";
        user = "bors-ng";
      };
    };
  };
  log-classifier = { config, pkgs, resources, ... }: let
    hostName = "log-classifier.aws.iohkdev.io";
    keysDir = "/var/lib/keys";
  in {
    imports = [
      ../modules/log-classifier.nix
      ../modules/common.nix
    ];

    services.log-classifier.domain = "log-classifier.aws.iohkdev.io";
  };

  monitoring = { lib, config, pkgs, resources, ... }: {
    imports = [
      ../modules/monitoring-services.nix
      ../modules/exchange-monitor.nix
    ];

    deployment.keys."monitoring.wgprivate" = {
      destDir = "/etc/wireguard";
      keyFile = ../static/monitoring.wgprivate;
    };
    networking.wireguard.interfaces.wg0 = {
      peers = [
        # hydra master
        (genPeer 2 ../static/hydra.wgpublic)
        # main hydra build-slaves
        (genPeer 11 ../static/packet-hydra-slave-1.wgpublic)
        (genPeer 12 ../static/packet-hydra-slave-2.wgpublic)
        (genPeer 13 ../static/packet-hydra-slave-3.wgpublic)
        (genPeer 14 ../static/packet-hydra-slave-4.wgpublic)
        (genPeer 15 ../static/packet-hydra-slave-5.wgpublic)
        # nixos+mac machines
        (genPeer 20 ../static/sarov.wgpublic)
        (genPeer 21 ../static/mac-mini-1.wgpublic)
        (genPeer 22 ../static/mac-mini-2.wgpublic)
        # buildkite agents
        (genPeer 31 ../static/packet-buildkite-1.wgpublic)
        (genPeer 32 ../static/packet-buildkite-2.wgpublic)
        (genPeer 33 ../static/packet-buildkite-3.wgpublic)
        # mantis hydra build-slaves
        (genPeer 51 ../static/mantis-slave-packet-1.wgpublic)
        (genPeer 52 ../static/mantis-slave-packet-2.wgpublic)
      ];
    };

    services = {
      exchange-monitor.enable = true;
      prometheus2.scrapeConfigs = [
        #{
        #  job_name = "sarov-host";
        #  scrape_interval = "10s";
        #  metrics_path = "/mac1/host";
        #  static_configs = [
        #    {
        #      targets = [
        #        "192.168.20.20:9111"
        #      ];
        #      labels.role = "mac-host";
        #    }
        #  ];
        #}
        {
          job_name = "mac-mini-1-host";
          scrape_interval = "10s";
          metrics_path = "/monitorama/host";
          static_configs = [
            {
              targets = [
                "192.168.20.21:9111"
              ];
              labels.role = "mac-host";
            }
          ];
        }
        {
          job_name = "mac-mini-2-host";
          scrape_interval = "10s";
          metrics_path = "/monitorama/host";
          static_configs = [
            {
              targets = [
                "192.168.20.22:9111"
              ];
              labels.role = "mac-host";
            }
          ];
        }
        #{
        #  job_name = "sarov-mac-hydra";
        #  scrape_interval = "10s";
        #  metrics_path = "/mac1/hydra";
        #  static_configs = [
        #    {
        #      targets = [
        #        "192.168.20.20:9111"
        #      ];
        #      labels.role = "build-slave";
        #    }
        #  ];
        #}
        {
          job_name = "mac-mini-1-ci";
          scrape_interval = "10s";
          metrics_path = "/monitorama/ci";
          static_configs = [
            {
              targets = [
                "192.168.20.21:9111"
              ];
              labels.role = "build-slave";
            }
          ];
        }
        {
          job_name = "mac-mini-2-ci";
          scrape_interval = "10s";
          metrics_path = "/monitorama/ci";
          static_configs = [
            {
              targets = [
                "192.168.20.22:9111"
              ];
              labels.role = "build-slave";
            }
          ];
        }
        {
          job_name = "mac-mini-1-signing";
          scrape_interval = "10s";
          metrics_path = "/monitorama/signing";
          static_configs = [
            {
              targets = [
                "192.168.20.21:9111"
              ];
              labels.role = "build-slave";
            }
          ];
        }
        {
          job_name = "mac-mini-2-signing";
          scrape_interval = "10s";
          metrics_path = "/monitorama/signing";
          static_configs = [
            {
              targets = [
                "192.168.20.22:9111"
              ];
              labels.role = "build-slave";
            }
          ];
        }
      ];
      monitoring-services = {
        enable = true;
        enableWireguard = true;
        # NOTE: The Grafana user and password settings only take effect on the initial deployment.
        grafanaCreds = makeCreds "grafana" { user = "changeme"; password = "changeme"; };
        graylogCreds = makeCreds "graylog" { user = "changeme"; password = "changeme"; };
        grafanaAutoLogin = true;
        applicationRules = [
          {
            alert = "exchange-down-binance";
            expr = "binance_withdraws == 0 or binance_deposits == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-bittrex";
            expr = "bittrex_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-bitthumb";
            expr = "bithumb_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-hitbtc";
            expr = "hitbtc_withdraws == false or hitbtc_deposits == false";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-coinex";
            expr = "coinex_withdraws == false or coinex_deposits == false";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-bitmax";
            expr = "bitmax_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
           {
            alert = "exchange-down-bkex";
            expr = "bkex_withdraws == false or bkex_deposits == false";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
           }
          {
            alert = "exchange-down-bitrue";
            expr = "bitrue_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-exx";
            expr = "exx_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }
          {
            alert = "exchange-down-mxc";
            expr = "mxc_active == 0";
            for = "10m";
            labels.severity = "exchange-down";
            annotations = {
              description = "{{$labels.alias}} withdraws/deposits down for >=10mins";
            };
          }    
        ];
        alertmanager = {
          extraRoutes = [
            {
              match.severity = "exchange-down";
              receiver = "exchange-down";
            }
          ];
          extraReceivers = [
            {
              name = "exchange-down";
              pagerduty_configs = [
                {
                  service_key = if (builtins.pathExists ../static/pager-duty.nix)
                    then ((import ../static/pager-duty.nix).exchangeKey)
                    else { exchangeKey = null; };
                }
              ];
            }
          ];
        };
      };
    };
  };

  packet-hydra-slave-1 = mkUplink 11 ../static/packet-hydra-slave-1.wgprivate;
  packet-hydra-slave-2 = mkUplink 12 ../static/packet-hydra-slave-2.wgprivate;
  packet-hydra-slave-3 = mkUplink 13 ../static/packet-hydra-slave-3.wgprivate;
  packet-hydra-slave-4 = mkUplink 14 ../static/packet-hydra-slave-4.wgprivate;
  packet-hydra-slave-5 = mkUplink 15 ../static/packet-hydra-slave-5.wgprivate;

  packet-buildkite-1 = mkUplink 31 ../static/packet-buildkite-1.wgprivate;
  packet-buildkite-2 = mkUplink 32 ../static/packet-buildkite-2.wgprivate;
  packet-buildkite-3 = mkUplink 33 ../static/packet-buildkite-3.wgprivate;

  mantis-slave-packet-1 = mkUplink 51 ../static/mantis-slave-packet-1.wgprivate;
  mantis-slave-packet-2 = mkUplink 52 ../static/mantis-slave-packet-2.wgprivate;
}
