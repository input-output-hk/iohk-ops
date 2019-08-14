
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
  ];

  defaults = {
    services.monitoring-exporters.graylogHost = "192.168.20.1";
    imports = [
      ../modules/common.nix
      ../modules/globals.nix
    ];
  };

  resources.packetKeyPairs = mkPacketKeyPairs [ "ci" ];

  hydra = mkPacketNet {
    hostname = "hydra";
    type = "demand";
    module = ../modules/hydra-master-common.nix;
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    extraopts = {
      imports = [ ../modules/hydra-master-main.nix ];
    };
  };

  monitoring  = mkPacketNet {
    hostname = "monitoring";
    type = "demand";
    module = {};
    facility = "ams1";
    plan = "c2.medium.x86";
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
}
