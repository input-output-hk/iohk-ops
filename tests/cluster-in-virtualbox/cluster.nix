let
  sources = import ../../nix/sources.nix;
  inherit (import ../.. {}) cardano-node-pkgs cardano-node-srcs;
  systemStart = 1000000000;
  topologyYaml = ../../topology-mainnet-ci.yaml;
  cores = [ "c-a-1" "c-a-2" "c-b-1" "c-b-2" "c-c-1" "c-c-2" "c-d-1" ];
  relays = [ "r-a-1" ];
  names = cores ++ relays;
  mkProxyFollowerTopology =
    with builtins;
    { hostAddr
    , proxyPort
    , nodePort
    }:
    let
      topology = [{
        nodeId = 0;
        nodeAddress = {
          addr = hostAddr;
          port = nodePort;
        };
        producers = [
          { addr = hostAddr;
            port = proxyPort;
            valency = 1;
          }
        ];
      }];
    in toFile "proxy-follower-topology.yaml" (toJSON topology);
in
{

  defaults = { ... }: {
    deployment.targetEnv = "virtualbox";
    deployment.virtualbox.memorySize = 1024;
  };

  a-node = { pkgs, config, lib, ... }: with builtins; with lib;
    let
      preseedDB = pkgs.runCommand "node-db-seed" {} ''
        mkdir $out
        tar xaf ${../../static/node-db.mainnet.tar.xz} -C $out
      ''; in {
    imports = [
      (cardano-node-srcs + "/nix/nixos/cardano-node-service.nix")
      # (cardano-node-srcs + "/nix/nixos/cardano-cluster-service.nix")
      # (cardano-node-srcs + "/nix/nixos/chairman-as-a-service.nix")
      # (cardano-node-srcs + "/nix/nixos/cardano-node-legacy-service.nix")
      (sources.cardano-byron-proxy + "/nix/nixos")
      ../../modules/cardano-service.nix
      ../../modules/ntp-fudge.nix
      ../../modules/vde-lan-service.nix
    ];
    environment.systemPackages = with pkgs; [ tcpdump vde2 ];

    services.timesyncd.enable = false;
    services.ntp-fudge.enable = true;
    services.vde-lan = {
      enable = true;
      netaddr = "10.1.0.%d";
      netmask = "255.255.255.0";
      strength = 8;
    };

    services.dnsmasq.enable = true;
    services.dnsmasq.servers = [ "127.0.0.1" ];
    networking.extraHosts = ''
      10.1.0.1 c-a-1.cardano
      10.1.0.2 c-a-2.cardano
      10.1.0.3 c-b-1.cardano
      10.1.0.4 c-b-2.cardano
      10.1.0.5 c-c-1.cardano
      10.1.0.6 c-c-2.cardano
      10.1.0.7 c-d-1.cardano
      10.1.0.8 r-a-1.cardano
    '';

    deployment.keys = with lib; listToAttrs
      (map
        ({ i, name }: nameValuePair "key${toString i}.sk" {
          keyFile = cardano-node-srcs + "/configuration/mainnet-ci/key${toString i}.sk";
          user = "cardano-node";
          destDir = "/var/lib/keys/cardano-node";
        })
        (imap0 (i: name: { inherit i name; }) cores));

    services.cardano-node-legacy = {
      enable = true;
      instanced = true;
      inherit names;
      inherit topologyYaml systemStart; # preseedDB
      # privateIP = "127.0.0.1";
      publicIP  = null;
      nodeIndex = 0;          ## doesn't really matter
      name      = "whatever"; ## ..either
      nodeType  = "core";     ## ...
    };
    deployment.arguments.configurationKey = "mainnet_ci_full";
    # systemctl stop systemd-journald; systemctl stop cardano-node-legacy@{0,1,2,3,4,5,6,7}; rm /var/lib/cardano-node/* /var/log/journal -rf; date --set=@1000000000; systemctl start systemd-journald; systemctl start cardano-node-legacy@{0,1,2,3,4,5,6,7}

    services.byron-proxy = {
      enable = true;
      environment = "mainnet-ci";
      topologyFile = toFile "proxy-topology.yaml" ''
        wallet:
          relays: [[{ host: r-a-1.cardano, port: 3000 }]]
        '';
    };

    services.cardano-node = {
      enable = true;
      topology = #(import (cardano-node-srcs + "/lib.nix")).
        mkProxyFollowerTopology
        { hostAddr = "127.0.0.1";
          proxyPort = 7777;
          nodePort = 3001;
        };
      environment = "mainnet-ci";
      stateDir = "/var/lib/cardano-node/proxy-follower";
    };
    # services.chairman.enable = true;
    # services.cardano-cluster = {
    #   enable = true;
    #   node-count = 3;
    # };
  };
}
