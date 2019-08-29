{ lib, ... }:
with lib;
fix (self: let
  projects = self.packetSecrets.projects;
  accessKeyId = self.packetSecrets.accessKeyId;
in {
  mockCreds = {
    accessKeyId = "abc123";
    projects = {
      infra = {
        id = "1";
      };
      ci = {
        id = "2";
      };
    };
  };
  packetSecrets = if builtins.pathExists ../static/packet.nix then import ../static/packet.nix else self.mockCreds;
  getPacketKeyPairName = project: "packet-keypair-${project}";
  mkPacketKeyPair = project: {
    name = "packet-keypair-${project}";
    value = {
      inherit accessKeyId;
      project = projects.${project}.id;
    };
  };
  mkPacketKeyPairs = projects: listToAttrs (map self.mkPacketKeyPair projects);
  mkPacketNet = { hostname, modules, type ? "demand", facility ? "any", plan ? "c1.small.x86", ipxeScriptUrl ? "", project ? "infra", extraopts ? {} }:
  { config, name, pkgs, resources, ... }: let
    projectId = projects.${project}.id;
  in {
    deployment = let
      deploymentTypes = {
        demand = {
          targetHost = hostname + ".aws.iohkdev.io";
          targetEnv = "packet";
          packet = {
            inherit accessKeyId facility plan ipxeScriptUrl;
            keyPair = resources.packetKeyPairs.${self.getPacketKeyPairName project};
            project = projectId;
          };
        };
        legacy = {
          targetHost = hostname + ".aws.iohkdev.io";
        };
      };
    in deploymentTypes.${type};

    imports = modules ++ [
                extraopts
              ] ++ (optionals (type == "legacy")
              (map (f: ./. + "/${hostname}/${f}")
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")))));
    environment.systemPackages = with pkgs;
      [ moreutils ethtool jq ];
  };

  mkPacketNetMod = { hostname
                   , modules
                   , type ? "demand"
                   , facility ? "any"
                   , plan ? "c1.small.x86"
                   , ipxeScriptUrl ? ""
                   , project ? "infra"
                   , monitorDefault ? 0                              # When using multiple monitors, set this to the default one in the list
                   , monitoringWg ? [ { monName = "monitoring"; monWgIp = "192.168.20.1"; endpoint = "monitoring.ci.iohkdev.io:51820"; } ]
                   , centralWg ? [ { centralName = "hydra"; centralWgIp = "192.168.20.2"; endpoint = "hydra.ci.iohkdev.io:51820"; } ]
                   , peerWg ? []
                   , subnet ? "192.168.20"
                   , ipo4                                            # Octet 4 of the wireguard ip assignment
                   , extraopts ? {}
                   }:
  { config, name, pkgs, resources, lib, ... }: let
    projectId = projects.${project}.id;
  in {
    imports = modules ++ [
                extraopts
              ] ++ (optionals (type == "legacy")
              (map (f: ./. + "/${hostname}/${f}")
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")))));

    deployment = let
      deploymentTypes = {
        demand = {
          targetHost = hostname + ".ci.iohkdev.io";
          targetEnv = "packet";
          packet = {
            inherit accessKeyId facility plan ipxeScriptUrl;
            keyPair = resources.packetKeyPairs.${self.getPacketKeyPairName project};
            project = projectId;
          };
          keys."uplink.wgprivate" = {
            destDir = "/etc/wireguard";
            keyFile = ../static + "/${hostname}.wgprivate";
          };
        };
        legacy = {
          targetHost = hostname + ".ci.iohkdev.io";
        };
      };
    in deploymentTypes.${type};

    services.monitoring-exporters = {
      graylogHost = lib.mkForce "${(builtins.elemAt monitoringWg monitorDefault).monWgIp}:5044";
      ownIp = lib.mkForce "${subnet}.${toString ipo4}";
    };

    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];
    networking.firewall.allowedUDPPorts = [ 51820 ];

    networking.wireguard.interfaces.wg0 = {
      ips = [ "${subnet}.${toString ipo4}/24" ];
      listenPort = 51820;
      privateKeyFile = "/etc/wireguard/uplink.wgprivate";
      peers = let
        centralServer = { centralName, centralWgIp, endpoint }:
          {
            allowedIPs = [ "${centralWgIp}/32" ];
            publicKey = lib.strings.removeSuffix "\n" (builtins.readFile (../static + "/${centralName}.wgpublic"));
            endpoint = endpoint;
            persistentKeepalive = 30;
          };
        monServer = { monName, monWgIp, endpoint }:
          {
            allowedIPs = [ "${monWgIp}/32" ];
            publicKey = lib.strings.removeSuffix "\n" (builtins.readFile (../static + "/${monName}.wgpublic"));
            endpoint = endpoint;
            persistentKeepalive = 30;
          };
        peer = { allowedIPs, peerHostname, endpoint ? null }:
          {
            inherit allowedIPs;
            publicKey = lib.strings.removeSuffix "\n" (builtins.readFile (../static + "/${peerHostname}.wgpublic"));
            persistentKeepalive = 30;
          } // optionalAttrs (endpoint != null) endpoint;
      in (builtins.concatLists [
        (optionals (centralWg != {}) (map centralServer centralWg))
        (optionals (monitoringWg != {}) (map monServer monitoringWg))
        (optionals (peerWg != {}) (map peer peerWg))
      ]);
    };

    environment.systemPackages = with pkgs; [ moreutils ethtool jq ];
  };

  createPacketHydra = { hostname, ... }@args: self.mkPacketNetMod ({
    inherit hostname;
    type = "demand";
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    modules = [ ../modules/hydra-master-common.nix ];
    extraopts = {
      imports = [ ../modules/hydra-master-main.nix ];
    };
  } // args);

  createPacketMonitor = { hostname, ... }@args: self.mkPacketNetMod ({
    inherit hostname;
    type = "demand";
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    modules = [];
  } // args);

  createPacketHydraSlaveBuildkite = { hostname, ... }@args: self.mkPacketNetMod ({
    inherit hostname;
    type = "demand";
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    modules = [
      ../modules/hydra-slave.nix
      ../modules/buildkite-agent.nix
    ];
  } // args);

  createPacketHydraSlave = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/hydra-slave.nix ];
  };
  createPacketBuildkite = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/buildkite-agent.nix ];
  };
  createPacketHydraSlaveImpure = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/hydra-slave.nix ];
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketHydraSlaveImpureLegacy = hostname: mkPacketNet {
    inherit hostname;
    type = "legacy";
    modules = [ ../modules/hydra-slave.nix ];
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketMachines = function: hostname: { name = hostname; value = function hostname; };

  createPacketMachinesMod = function: { hostname, ... }@args: { name = hostname; value = function args; };
})
