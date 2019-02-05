{ config, pkgs, lib, ... }:

with lib;

let
  localLib = import ../../../lib.nix;
  cfg = config.services.mantis;
  otherNodes = self:
             let
              asMap = vs: builtins.listToAttrs (map (v: { name = v.name; value = v; }) vs);
              xs = (pkgs.lib.attrsets.filterAttrs (n: v: n != self) ((asMap cfg.machines.zoneA) // (asMap cfg.machines.zoneB) // (asMap cfg.machines.zoneC)));
              ys = (pkgs.lib.attrsets.filterAttrs (n: v: n != self) cfg.nodeIds);
              zs = (pkgs.lib.recursiveUpdate xs ys);
             in
              pkgs.lib.attrsets.attrValues zs;
  enode = node: "enode://${node.id}@${node.ip}:9076";
  localNode = node: "${node.ip}:0.0.0.0:5679";
  bootstrapEnodes = self: builtins.toJSON (map enode (otherNodes self));
  bootstrapNodes = self: builtins.toJSON (map (n: "${n.ip}:${n.ip}:5679") (otherNodes self));
  mantis_conf = pkgs.writeTextFile {
    name = "mantis.conf";
    text = ''
    include "application.conf"
    mantis.vm {
      mode = "external"
       external {
        vm-type = "${cfg.vmType}"
        run-vm = false
        host = "localhost"
        port = 8888
      }
    }
   mantis {
      consensus {
        protocol = atomix-raft

        mining-enabled = true

        # Minimum gas price to include the transaction in block to mine.
        # Note, that a transaction is still valid even if it's gas price is lower (it could be mined by another node).
        min-gas-price = 5000000000
      }

      atomix-raft {
        election-timeout = 30.seconds

        block-forging-delay = 15.seconds

        # The format is: ID:IP:PORT
        local-node = "${localNode cfg.node}"

        # Each list item has the same format as the `local-node`
        bootstrap-nodes = ${bootstrapNodes cfg.nodeName}
      }
      datadir = ${cfg.dataDir}

      ${optionalString (cfg.riemannHost != null) ''
      riemann {
         host = ${cfg.riemannHost}
         port = 5555
         buffer-size = 1000
         batch-size = 10
         auto-flush-ms = 1000
         # optional hostname to send to Riemann server, if not provided the client will use InetAddress.getLocalHost().getHostName()
         host-name = ${cfg.nodeName}
       }
       ''}

      network {
        peer {
          # Ethereum network identifier:
          # 1 - mainnet, 2 - morden
          network-id = ${cfg.machines.networkId}
        }
        rpc {
          http {
            # JSON-RPC mode
            # Available modes are: http, https
            # Choosing https requires creating a certificate and setting up 'certificate-keystore-path' and
            # 'certificate-password-file'
            # See: https://github.com/input-output-hk/mantis-cardano/wiki/Creating-self-signed-certificate-for-using-JSON-RPC-with-HTTPS
            mode = "http"
            # Listening address of JSON-RPC HTTP/HTTPS endpoint
            interface = ''${mantis.network.server-address.interface}
            # Domains allowed to query RPC endpoint. Use "*" to enable requests from
            # any domain.
            cors-allowed-origins = "*"
            apis = "eth,web3,net,iele"
            disabled-methods = [
              "iele_sendTransaction",
              "eth_accounts",
              "eth_sendTransaction",
              "eth_sign",
              "net_peerCount",
              "net_listening",
              "eth_syncing",
              "eth_hashrate",
              "eth_mining",
              "eth_getWork",
              "eth_submitWork",
              "eth_coinbase"
            ]
          }
        }
        server-address {
          interface = "0.0.0.0"

          # Listening port for Ethereum protocol connections
          # port = 9076
        }
        discovery {
          discovery-enabled = true

          interface = ''${mantis.network.server-address.interface}

          # Listening port for discovery protocol
          # Note that this is a UDP port
          # port = 30303

          bootstrap-nodes = ${bootstrapEnodes cfg.nodeName}
        }
      }
      metrics {
        # Set to `true` iff your deployment supports metrics collection.
        # We push metrics to a StatsD-compatible agent and we use Datadog for collecting them in one place.
        # We default to `false` here because we do not expect all deployments to support metrics collection.
        enabled = true

        # The StatsD-compatible agent host.
        host = "localhost"

        # The StatsD-compatible agent port (UDP).
        # port = 8125

        # Size of the metrics requests queue.
        # If the queue contains that many outstanding requests to the metrics agent, then
        # subsequent requests are blocked until the queue has room again.
        # queue-size = 1024

        # Iff true, any errors during metrics client operations will be logged.
        # log-errors = true
      }
      sync {
        # Whether to enable fast-sync
        do-fast-sync = false
        # Duration for blacklisting a peer. Blacklisting reason include: invalid response from peer, response time-out, etc.
        # 0 value is a valid duration and it will disable blacklisting completely (which can be useful when all nodes are
        # are controlled by a single party, eg. private networks)
        blacklist-duration = 0
        # Set to false to disable broadcasting the NewBlockHashes message, as its usefulness is debatable,
        # especially in the context of private networks
        broadcast-new-block-hashes = false
      }
      blockchain {
        # DAO fork configuration (Ethereum HF/Classic split)
        # https://blog.ethereum.org/2016/07/20/hard-fork-completed/
        dao = null
        # Custom genesis JSON file
        # null value indicates using default genesis definition that matches the main network
        custom-genesis-file = "/etc/mantis/genesis.json"
        # if true, account storage will use Ethereum-specific format for storing keys/value in MPT (32 byte)
        # if false, generic storage for arbitrary length integers will be used
        eth-compatibility-mode = ${lib.boolToString cfg.ethCompatibilityMode}
        frontier-block-number = ${toString cfg.frontierBlockNumber}
        homestead-block-number = ${toString cfg.homesteadBlockNumber}
        eip106-block-number = ${toString cfg.eip106BlockNumber}
        eip150-block-number = ${toString cfg.eip150BlockNumber}
        eip155-block-number = ${toString cfg.eip155BlockNumber}
        eip160-block-number = ${toString cfg.eip160BlockNumber}
        eip161-block-number = ${toString cfg.eip161BlockNumber}
        difficulty-bomb-pause-block-number = ${toString cfg.difficultyBombPauseBlockNumber}
        difficulty-bomb-continue-block-number = ${toString cfg.difficultyBombContinueBlockNumber}

        monetary-policy {
          first-era-block-reward = ${toString cfg.monetaryPolicyFirstEraBlockReward}
          era-duration = ${toString cfg.monetaryPolicyEraDuration}
          reward-reduction-rate = ${toString cfg.monetaryPolicyRewardReductionRate}
        }

        # specify constant gas limit for all blocks in the blockchain
        # if not provided (null) the default Ethereum formula will be applied (based on genesis block)
        constant-block-gas-limit = 8000000
      }
    }
    '';
  };
  mkAddress = faucetNode: pkgs.lib.attrsets.nameValuePair faucetNode { "balance" = "1606938044258990275541962092341162602522202993782792835301376"; };
  genesisAddresses = builtins.listToAttrs [ (mkAddress cfg.faucetAddresses.faucetA) (mkAddress cfg.faucetAddresses.faucetB) (mkAddress cfg.faucetAddresses.faucetC) ];
  genesis_json = pkgs.writeTextFile {
    name = "genesis.json";
    text = ''
    {
      "extraData": "0x00",
      "nonce": "0x0000000000000042",
      "gasLimit": "0x2fefd8",
      "difficulty": "0x400",
      "ommersHash": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
      "timestamp": "0x00",
      "coinbase": "0x0000000000000000000000000000000000000000",
      "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
      "alloc": ${builtins.toJSON genesisAddresses}
    }
    '';
  };
  logback_xml = pkgs.writeTextFile {
    name = "logback.xml";
    text = ''
    <configuration>
        <property name="stdoutEncoderPattern" value="%d{HH:mm:ss.SSS} [%thread] %-5level %logger %X{akkaSource} - %msg%n" />
        <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
            <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
                <level>DEBUG</level>
            </filter>
            <encoder>
                <pattern>''${stdoutEncoderPattern}</pattern>
            </encoder>
        </appender>
        <root level="${cfg.logbackLevel}">
            <appender-ref ref="STDOUT" />
        </root>
        ${cfg.logbackAdditions}
    </configuration>
    '';
  };
in
{
  options.services.mantis = {
    enable = mkOption {
      description = "Whether to enable the mantis service";
      default = false;
      type = types.bool;
    };

    node = mkOption {
      description = "This mantis node";
      type = types.attrs;
    };

    nodeName = mkOption {
      description = "This mantis node name";
      example = "mantis-a-0";
      type = types.str;
    };

    machines = mkOption {
      description = "All the testnet machines";
      type = types.attrs;
    };

    logbackAdditions = mkOption {
      description = "Any additions to the logback.xml file";
      default = "";
      type = types.str;
    };

    logbackLevel = mkOption {
      description = "The root logger level";
      default = "DEBUG";
      type = types.enum [ "TRACE" "DEBUG" "INFO" "WARN" "ERROR" ];
    };

    dataDir = mkOption {
      description = "Where mantis stores its data";
      type = types.str;
    };

    jvmOptions = mkOption {
      description = "Additional JVM options";
      example = "-J-Xss10M";
      default = "-J-Xss10M";
      type = types.str;
    };

    nodeIds = mkOption {
      description = "The mantis node ids";
      type = types.attrs;
    };

    faucetAddresses = mkOption {
      description = "The faucet addresses";
      type = types.attrs;
    };

    vmType = mkOption {
      description = "iele or kevm";
      type = types.enum [ "iele" "kevm" ];
    };

    mantisPkg = mkOption {
      type = types.package;
      description = "The mantis package to use, e.g. hydra.mantis";
    };

    vmPkg = mkOption {
      type = types.package;
      description = "The VM package to use, e.g. hydra.iele";
    };

    ethCompatibilityMode = mkOption {
      type = types.bool;
      description = "should be true for KEVM and false for IELE";
    };

    frontierBlockNumber = mkOption {
      type = types.int;
    };

    homesteadBlockNumber = mkOption {
      type = types.int;
    };

    eip106BlockNumber = mkOption {
      type = types.int;
    };

    eip150BlockNumber = mkOption {
      type = types.int;
    };

    eip155BlockNumber = mkOption {
      type = types.int;
    };

    eip160BlockNumber = mkOption {
      type = types.int;
    };

    eip161BlockNumber = mkOption {
      type = types.int;
    };

    difficultyBombPauseBlockNumber = mkOption {
      type = types.int;
    };

    difficultyBombContinueBlockNumber = mkOption {
      type = types.int;
    };

    monetaryPolicyFirstEraBlockReward = mkOption {
      type = types.int;
    };

    monetaryPolicyEraDuration = mkOption {
      type = types.int;
    };

    monetaryPolicyRewardReductionRate = mkOption {
      type = localLib.nixpkgs.lib.types.float;
    };

    riemannHost = mkOption {
      type = types.str;
      description = "DNS address or IP of the Riemann server";
    };

  };

  config = mkIf cfg.enable {

    environment.etc = [ { source = mantis_conf; target = "mantis/mantis.conf"; }
                        { source = logback_xml; target = "mantis/logback.xml"; }
                        { source = genesis_json; target = "mantis/genesis.json"; }
                      ];

    networking.firewall = {
      enable = true;
      allowedUDPPorts = [ 8125 ];
      allowedTCPPorts = [ 8546 9076 5679 30303 5555 ];
    };

    users.users.mantis =
        { isNormalUser = true;
          home = "/home/mantis";
          description = "Mantis user";
          extraGroups = [ "systemd-journal" ];
          openssh.authorizedKeys.keys = cfg.machines.mantisSshKeys;
        };

    fileSystems."/data" = {
      device = "/dev/xvdh";
      fsType = "ext4";
      autoFormat = true;
    };

    systemd.services.mantis = {
      requires = [ "${cfg.vmType}.service" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig.RequiresMountsFor = "/data";
      enable = true;
      path = [
        (builtins.storePath cfg.mantisPkg)
        (builtins.storePath pkgs.openjdk8)
        (builtins.storePath pkgs.gawk)
        (builtins.storePath pkgs.gnused)
      ];
      serviceConfig = {
        PermissionsStartOnly = true;
        TimeoutStartSec = "0";
        Restart = "always";
      };
      preStart = ''
        mkdir -p ${cfg.dataDir}
        if [ ! -f ${cfg.dataDir}/node.key ]; then
           echo "no key found, copying key from /root/keys.txt to ${cfg.dataDir}/node.key"
           cp /root/keys.txt ${cfg.dataDir}/node.key
        else
           echo "key found at ${cfg.dataDir}/node.key, no action required"
        fi;
        chown -R mantis ${cfg.dataDir}
      '';
       script = "mantis mantis -Dconfig.file=/etc/mantis/mantis.conf -Dlogback.configurationFile=/etc/mantis/logback.xml ${cfg.jvmOptions}";
       restartTriggers = [ mantis_conf logback_xml genesis_json ];
    };

    systemd.services."${cfg.vmType}" = {
      enable = true;
      path = [
        (builtins.storePath cfg.vmPkg)
      ];
       serviceConfig = {
        TimeoutStartSec = "0";
        Restart = "always";
      };
       script = "${cfg.vmType}-vm 8888 0.0.0.0";
    };
  };
}
