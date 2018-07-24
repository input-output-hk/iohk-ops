{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.cardano-faucet;

  iohkPkgs = import ../default.nix { inherit config pkgs; inherit (pkgs) system; };
  walletPackage = iohkPkgs.cardano-sl-wallet-new-static;
  faucetPackage = iohkPkgs.cardano-sl-faucet-static;
  toolsPackage = iohkPkgs.cardano-sl-tools;
  configFiles = iohkPkgs.cardano-sl-config;

  walletStateDir = "${cfg.home}/wallet";
  tlsDir = "${cfg.home}/tls";
  walletPort = 8090;
  walletUseTLS = true;
  faucetEkgPort = 8001;  # currently hardcoded in faucet
  walletEkgPort = 8002;

  defaultLoggingCfg = pkgs.writeText "logging.cfg" ''
    termSeveritiesErr: All
    loggerTree:
      severity: Info+      # severities for root logger
      withdraw:            # logger for the withdraw action
        severity: Debug+   # severities withdraw logger
  '';

  cfgFile = pkgs.writeText "config.json" (builtins.toJSON ({
    wallet-host = "127.0.0.1";
    wallet-port = walletPort;
    inherit (cfg) port;
    payment-distribution = {
      mean = cfg.paymentAmountMean;
      scale = cfg.paymentAmountScale;
    };
    logging-config = cfg.loggingConfigFile;
    source-wallet.generate-to = "${cfg.home}/generated-wallet.json";
  } // (optionalAttrs (cfg.recaptcha != null) {
    # fixme: recaptcha secret in file
    recaptcha-secret = cfg.recaptcha.secretKey;
  }) // (optionalAttrs cfg.statsd.enable {
    statsd = { inherit (cfg.statsd) host port flush-interval; };
  }) // (optionalAttrs walletUseTLS {
    public-certificate = "${tlsDir}/client/client.crt";
    private-key = "${tlsDir}/client/client.pem";
  })));

  topologyFile = pkgs.writeText "topology.yaml" ''
    wallet:
      relays: [[{ host: ${cfg.relayHost} }]]
      valency: 1
      fallbacks: 7
  '';

in {
  options.services.cardano-faucet = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    home = mkOption {
      description = "Home directory of cardano-faucet user";
      type = types.str;
      default = "/var/lib/cardano-faucet";
    };

    paymentAmountMean = mkOption {
      description = ''
        Withdrawal amounts are random. This is the mean amount to
        withdraw in lovelace (millions of ADA).
      '';
      type = types.ints.positive;
      default = 1000000000;
    };

    paymentAmountScale = mkOption {
      description = ''
        Withdrawal amounts are random. The payment variation is the
        maximum difference between the mean (paymentAmountMean) and the
        actual amount dispensed.
        This value is in lovelace (millions of ADA).
      '';
      type = types.ints.positive;
      default = 500000000;
    };

    port = mkOption {
      type = types.int;
      default = 8081;
      description = "Port to serve the faucet API on.";
    };

    configKey = mkOption {
      type = types.str;
      description = "Which section of configuration.yaml to use for the wallet.";
    };

    relayHost = mkOption {
      type = types.str;
      description = "Relay hostname for wallet to connect to";
    };

    statsd = mkOption {
      description = "Statsd settings for cardano-faucet";
      type = types.submodule {
        options = {
          enable = mkOption {
            description = "Enable serving of statsd metrics.";
            type = types.bool;
            default = true;
          };
          host = mkOption {
            type = types.str;
            default = "127.0.0.1";
          };
          port = mkOption {
            type = types.int;
            default = 8125;
          };
          flush-interval = mkOption {
            type = types.int;
            default = 1000;
          };
        };
      };
      default = {
        enable = true;
        host = "127.0.0.1";
        port = 8125;
        flush-interval = 1000;
      };
    };

    loggingConfigFile = mkOption {
      description = "File containing a YAML logging config";
      type = types.path;
      default = defaultLoggingCfg;
    };

    recaptcha = mkOption {
      description = "Key pair for Google reCAPTCHA";
      type = types.nullOr (types.submodule {
        options = {
          siteKey = mkOption {
            description = ''
              The public reCAPTCHA site key which will be embedded in the frontend HTML.
            '';
            type = types.str;
          };
          secretKey = mkOption {
            description = ''
              The private reCAPTCHA key shared with Google during authentication.
            '';
            type = types.str;
          };
        };
      });
      default = null;
    };

    walletDebug = mkOption {
      description = "Enable debug mode of cardano-sl-wallet-new";
      type = types.bool;
      default = false;
    };

  };

  config = mkIf cfg.enable {

    users = {
      users.cardano-faucet = {
        group = "cardano-faucet";
        createHome = true;
        home = cfg.home;
        extraGroups = [ "keys" ];
      };
      groups.cardano-faucet = {};
    };

    systemd.services = {
      cardano-faucet-wallet-certs = {
        description = "Generate SSL certificates for wallet API";
        serviceConfig = {
          Type = "oneshot";
          User = "cardano-faucet";
          Group = "cardano-faucet";
        };
        script = ''
          if [ -f ${tlsDir}/server/server.crt ]; then
            echo "Wallet SSL certificates already exist in ${tlsDir}"
          else
            echo "Creating SSL certificates in ${tlsDir}"
            mkdir -p ${tlsDir}/server ${tlsDir}/client
            ${toolsPackage}/bin/cardano-x509-certificates \
              --server-out-dir ${tlsDir}/server \
              --clients-out-dir ${tlsDir}/client \
              --configuration-key ${cfg.configKey} \
              --configuration-file ${configFiles}/lib/configuration.yaml
          fi
        '';
      };

      cardano-faucet-wallet = {
        description   = "Cardano cardano-faucet wallet";
        after         = [ "network.target" ] ++ optional walletUseTLS "cardano-faucet-wallet-certs.service";
        requires      = optional walletUseTLS "cardano-faucet-wallet-certs.service";
        wantedBy      = [ "multi-user.target" ];
        serviceConfig = {
          User = "cardano-faucet";
          Group = "cardano-faucet";
          # Allow a maximum of 5 retries separated by 30 seconds, in total capped by 200s
          Restart = "always";
          RestartSec = 30;
          StartLimitInterval = 200;
          StartLimitBurst = 5;
          KillSignal = "SIGINT";
          WorkingDirectory = walletStateDir;
          PrivateTmp = true;
          Type = "notify";
        };
        script = let
          tlsArgs = if walletUseTLS
            then "--tlscert ${tlsDir}/server/server.crt --tlskey ${tlsDir}/server/server.key --tlsca ${tlsDir}/server/ca.crt"
            else "--no-tls";
          ekgArgs = optionalString (walletEkgPort != null)
            "--ekg-server 127.0.0.1:${toString walletEkgPort}";
          metricsArgs = optionalString cfg.statsd.enable
            "--metrics +RTS -T -RTS --statsd-server ${cfg.statsd.host}:${toString cfg.statsd.port}";
        in ''
          exec ${walletPackage}/bin/cardano-node                            \
            --configuration-file ${configFiles}/lib/configuration.yaml      \
            --configuration-key ${cfg.configKey}                            \
            ${tlsArgs}                                                      \
            --log-config ${configFiles}/log-configs/connect-to-cluster.yaml \
            --topology ${topologyFile}                                      \
            --logs-prefix "${walletStateDir}/logs"                          \
            --db-path "${walletStateDir}/db"                                \
            --wallet-db-path "${walletStateDir}/wallet-db"                  \
            ${optionalString cfg.walletDebug "--wallet-debug"}              \
            --keyfile ${walletStateDir}/secret.key                          \
            --wallet-address 127.0.0.1:${toString walletPort}               \
            ${ekgArgs}                                                      \
            ${metricsArgs}
        '';
      };

      cardano-faucet = {
        description = "Cardano cardano-faucet";
        after       = [ "network.target" "cardano-faucet-wallet.service" ];
        wantedBy    = [ "multi-user.target" ];
        requires    = [ "cardano-faucet-wallet.service" ];
        serviceConfig = {
          User = "cardano-faucet";
          Group = "cardano-faucet";
          ExecStart = let
            metricsArgs = optionalString cfg.statsd.enable "+RTS -T -RTS";
            # fixme: faucet ekg currently hardcoded to 127.0.0.1:8001
          ekgArgs = optionalString (faucetEkgPort != null)
            "--ekg-server 127.0.0.1:${toString faucetEkgPort}";
          in "${faucetPackage}/bin/cardano-faucet --config ${cfgFile} ${metricsArgs}";
        };
      };
    };
  };
}
