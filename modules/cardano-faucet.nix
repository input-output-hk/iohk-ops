with (import ./../lib.nix);

params:
{ pkgs, config, resources, options, ...}:

let
  iohkpkgs = import ../. {};
  faucet-drv = iohkpkgs.cardano-sl-faucet-static;
  cfg = config.services.faucet;
  tlsPath = fileName: "${cfg.home}/wallet/tls/client/${fileName}";
  walletPort = cfg.faucet-config.wallet-port;
  ekgPort = 8001;
  loggingCfg = pkgs.writeText "logging.cfg"
    ''
    termSeveritiesErr: All
    loggerTree:
      severity: Info+   # severities for «root» logger
      withdraw:          # logger for the withdraw action
        severity: Debug+   # severities withdraw logger
    '';
  generateWalletOpts = {
    options = {
      generate-to = mkOption {
        type = types.path;
      };
    };
  };
  readWalletOpts = {
    options = {
      read-from = mkOption {
        type = types.path;
      };
    };
  };
  cfgFile = pkgs.writeText "config.json" (builtins.toJSON cfg.faucet-config);
  walletSource = "${cfg.home}/wallet-source.json";
  generatedWalletDefault = "${cfg.home}/generated-wallet.json";
  # walletCfg cribbed from cardano-benchmark
  walletCfg = {
    walletListen = "127.0.0.1:${toString walletPort}";
    ekgListen = "127.0.0.1:${toString ekgPort}";
    # TODO: how do environment and confKey interact?
    # from connect-to-cluster it looks like setting environment to override
    # means that you need to specify relays?
    # environment = cfg.environment;
    # confKey = "bench";
    stateDir = "${cfg.home}/wallet";
    # TODO: Is this needed?
    # extraParams = "--system-start ${toString cfg.systemStart}";
  };
in
{
    imports = [
      ./common.nix
      ./amazon-base.nix
      ./network-wide.nix
      ./datadog.nix
    ];

  options = {
    services.faucet = with types; {
      enable = mkOption { type = bool; default = true; };
      home = mkOption { type = string; default = "/var/lib/faucet"; };
      # TODO: Do we need this?
      environment = mkOption { type = string; default = "demo"; };

      faucet-config = mkOption {
        description = "Configuration for the faucet";
        type = submodule {
          options = {
            wallet-host = mkOption { type = str; default = "127.0.0.1"; };
            wallet-port = mkOption { type = int; default = 8090; };
            payment-distribution = mkOption {
              description = "Settings for the distribution of amount to pay out from the faucet";
              type = submodule {
                options = {
                  mean = mkOption { type = int; default = 500; };
                  scale = mkOption { type = int; default = 500; };
                };
              };
              default = { mean = 1000; scale = 500;};
            };
            port = mkOption {
              type = int;
              default = 8081 ;
              description = "Port to serve the faucet on";
            };
            statsd = mkOption {
              description = "Statsd settings";
              type = submodule {
                options = {
                  host = mkOption { type = str; default = "127.0.0.1"; };
                  port = mkOption { type = int; default = 8125; };
                  flush-interval = mkOption { type = int; default = 1000; };
                };
              };
              default = {
                host = "127.0.0.1";
                port = 8125;
                flush-interval = 1000;
              };
            };
            source-wallet = mkOption {
              type = either (submodule generateWalletOpts) (submodule readWalletOpts);
              default = { generate-to = generatedWalletDefault; };
            };
            logging-config = mkOption { type = path; default = loggingCfg; };
            public-certificate = mkOption { type = path; default = tlsPath "client.crt"; };
            private-key = mkOption { type = path; default = tlsPath "client.key"; };

          };
        };
      };
    };
  };

  config = {


    global = {
      organisation             = params.org;
      dnsHostname              = mkForce "faucet";
    };

    deployment.ec2.region         = mkForce params.region;
    deployment.ec2.accessKeyId    = params.accessKeyId;
    deployment.ec2.keyPair        = resources.ec2KeyPairs.${params.keyPairName};
    deployment.ec2.securityGroups = [ resources.ec2SecurityGroups."allow-all-${params.region}-${params.org}" ];

    deployment.keys."wallet-source.json" = {
      keyFile = ./. + "/../static/wallet-source.json";
      destDir = cfg.home;
      user = "faucet";
    };

    users = {
      users.faucet = {
        group = "faucet";
        createHome = true;
        home = cfg.home;
        extraGroups = [ "keys" ];
      };
      groups.faucet = {};
    };

    # Not sure if I need to start this myself or not?
    services.statsd = {
      enable = true;
      backends = ["graphite"];
      graphiteHost = "127.0.0.1";
      graphitePort = 2003;
    };

    systemd.services = {
      wallet = mkIf cfg.enable {
        description = "Cardano faucet wallet";
        after         = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = "faucet";
          Group = "faucet";
          ExecStart = iohkpkgs.connectScripts.staging.wallet.override walletCfg;
        };
      };
      faucet = mkIf cfg.enable {
        description = "Cardano faucet";
        after         = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        requires = [ "wallet.service"];
        serviceConfig = {
          User = "faucet";
          Group = "faucet";
        };
        script = ''
        ${faucet-drv}/bin/faucet --config ${cfgFile} +RTS -T
        '';
      };
    };


    networking.firewall.allowedTCPPorts = mkIf  cfg.enable [
      # TODO: Do I need to open ports to stuff that's only running on localhost?
      cfg.faucet-config.port ekgPort walletPort config.services.statsd.graphitePort
    ];

  };
}
