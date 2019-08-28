with import ../lib.nix;
let
  inherit (pkgs) lib;
  mkMac = { wgip
  , deployip # the IP used to reach it from the deployer
  , deployPort
  , hostname # the hostname
  , hostid # the hostid for zfs
  #, role
}: { pkgs, config, ... }: {
    imports = [ ../modules/mac-host-common.nix ];
    deployment = {
      targetHost = deployip;
      targetPort = deployPort;
      keys."private.key" = {
        destDir = "/etc/wireguard";
        keyFile = ../static + "/${hostname}.wgprivate";
      };
    };
    networking = {
      hostName = hostname;
      hostId = hostid;
      wireguard.interfaces.wg0.ips = [ "${wgip}/24" ];
    };
    fileSystems = lib.mkIf (hostname == "sarov") {
      "/".device = lib.mkForce "${hostname}/root";
      "/home".device = lib.mkForce "${hostname}/home";
      "/nix".device = lib.mkForce "${hostname}/nix";
    };
    #macosGuest.role = role;
  };
in lib.mapAttrs (hostname: metadata: mkMac { inherit (metadata) wgip deployip hostname hostid deployPort; }) (import ./mac-metadata.nix)

#{
#  sarov = mkMac {
#    wgip = "192.168.20.20"; # what ip to configure the machine at on wireguard
#    deployip = "192.168.20.20"; # what ip to ssh into when deploying
#    hostname = "sarov";
#    hostid = "d11ab455"; # the random hostid generated at install time
#  };
#}
