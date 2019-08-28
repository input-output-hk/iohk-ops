
with import ../lib.nix;
with import ./packet-lib.nix { inherit (pkgs) lib; };

let
  inherit (pkgs) lib;
  genPeer = n: path: {
    allowedIPs = [ "192.168.20.${toString n}/32" ];
    publicKey = lib.strings.removeSuffix "\n" (builtins.readFile path);
    persistentKeepalive = 30;
  };
in {
  require = [
    ./monitoring-env-production.nix
    ./hydra-master-wireguard.nix
    ./mac-base.nix
  ];

  defaults = {
    services.monitoring-exporters.graylogHost = "192.168.20.1";
    imports = [
      ../modules/common.nix
      ../modules/globals.nix
    ];
  };

  network = {
    description = "CI Infrastructure";
    enableRollback = true;
  };

  resources.packetKeyPairs = mkPacketKeyPairs [ "ci" ];

  hydra = mkPacketNet {
    hostname = "hydra";
    type = "demand";
    modules = [ ../modules/hydra-master-common.nix ];
    facility = "ams1";
    plan = "c2.medium.x86";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    project = "ci";
    extraopts = {
      imports = [ ../modules/hydra-master-main.nix ];
    };
  };

  monitoring = mkPacketNet {
    hostname = "monitoring";
    type = "demand";
    modules = [];
    facility = "ams1";
    plan = "c2.medium.x86";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    project = "ci";
    extraopts = {
      deployment.keys."monitoring.wgprivate" = {
        destDir = "/etc/wireguard";
        keyFile = ../static/monitoring.wgprivate;
      };
      networking.wireguard.interfaces.wg0 = {
        peers = [
          # hydra master
          (genPeer 2 ../static/hydra.wgpublic)
        ];
      };
      services.monitoring-services.enableWireguard = true;
    };
  };

  packet-hydra-buildkite-1 = mkPacketNet {
    hostname = "packet-hydra-buildkite-1";
    type = "demand";
    modules = [ 
      ../modules/hydra-slave.nix
      ../modules/buildkite-agent.nix
    ];
    facility = "ams1";
    plan = "c2.medium.x86";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    project = "ci";
  };

  packet-hydra-buildkite-2 = mkPacketNet {
    hostname = "packet-hydra-buildkite-2";
    type = "demand";
    modules = [ 
      ../modules/hydra-slave.nix
      ../modules/buildkite-agent.nix
    ];
    facility = "ams1";
    plan = "c2.medium.x86";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    project = "ci";
  };
}
