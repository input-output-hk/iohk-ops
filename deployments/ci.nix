{ domain, ... }:
with import ../lib.nix;
with import ./packet-lib.nix { inherit domain; inherit (pkgs) lib; };

let
  inherit (pkgs) lib;

  # All parameters in mkPacketNetMod are overridable from within each host provided attribute set

  monitorList = [
    { hostname = "monitoring"; ipo4 = "1"; inherit peerWg; }
  ];

  hydraList = [
    { hostname = "hydra"; ipo4 = "2"; inherit peerWg; }
  ];

  hydraSlaveBuildkiteList = [
    { hostname = "packet-hydra-buildkite-1";
      ipo4 = "11";
      extraopts = {
        services.buildkite-containers.hostIdSuffix = "1";
        services.hydra-slave.cores = 4;
      };
    }
    { hostname = "packet-hydra-buildkite-2";
      ipo4 = "12";
      extraopts = {
        services.buildkite-containers.hostIdSuffix = "2";
        services.hydra-slave.cores = 4;
      };
    }
  ];

  peerWg = [
    { allowedIPs = [ "192.168.20.11/32" ]; peerHostname = "packet-hydra-buildkite-1"; }
    { allowedIPs = [ "192.168.20.12/32" ]; peerHostname = "packet-hydra-buildkite-2"; }
  ];

in {
  require = [
    ./monitoring-env-production.nix
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
}
// builtins.listToAttrs (builtins.concatLists [
    (map (createPacketMachinesMod createPacketMonitor) monitorList)
    (map (createPacketMachinesMod createPacketHydra) hydraList)
    (map (createPacketMachinesMod createPacketHydraSlaveBuildkite) hydraSlaveBuildkiteList)
   ])
