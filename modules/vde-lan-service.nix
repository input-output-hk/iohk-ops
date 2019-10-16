{ config, pkgs, lib, options, ... }:
with builtins; with lib;
let
  cfg = config.services.vde-lan;
in {
  options = with types; {
    services.vde-lan = {
      enable = mkOption {
        type = bool;
        description = "Whether to enable the VDE LAN.";
        default = false;
      };

      tap-base = mkOption {
        type = int;
        description = "First allocated address.";
        default = 1;
      };

      netaddr = mkOption {
        type = str;
        description = "Network address pattern.";
        default = "10.1.0.%d";
      };

      netmask = mkOption {
        type = str;
        description = "Netmask of the VDE LAN";
        default = "255.255.255.0";
      };

      netsize = mkOption {
        type = str;
        description = "Bit size of the netmask";
        default = "24";
      };

      strength = mkOption {
        default = 1;
        type = int;
        description = ''List of name:ip pairs of neighbours.'';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.vde-lan = let
      tap-base = toString cfg.tap-base;
      ip = "${pkgs.iproute}/bin/ip";
    in {
      description   = "VDE LAN";
      after         = [ "network.target" ];
      wantedBy      = [ "multi-user.target" ];
      script = let
      in ''
        ${pkgs.vde2}/bin/vde_switch -daemon -tap tap${tap-base}
      '';
      postStart = let
      in ''
      set -x
      ${pkgs.nettools}/bin/ifconfig tap${tap-base} $(printf "${cfg.netaddr}" ${tap-base}) netmask ${cfg.netmask}

      addr=1
      for tap in $(seq $((${tap-base} + 1)) $((${tap-base} + ${toString cfg.strength} - 1)))
      do ${pkgs.vde2}/bin/vde_plug2tap -daemon tap$tap
         addr=$((addr + 1))
         ${pkgs.nettools}/bin/ifconfig tap$tap $(printf "${cfg.netaddr}" $addr) netmask ${cfg.netmask}
      done

      netaddr=$(printf "${cfg.netaddr}" 0)
      for tap in $(seq ${tap-base}        $((${tap-base} + ${toString cfg.strength} - 1)))
      do ${ip} route flush     $tap
         ${ip} route add table $tap to $netaddr/${cfg.netsize} dev tap$tap
         # ip  route add table $tap to default via 192.168.150.1 dev eth1
      done

      for tap in $(seq ${tap-base}        $((${tap-base} + ${toString cfg.strength} - 1)))
      do ${ip} rule add from $(printf "${cfg.netaddr}" $tap)/32 table $tap priority $tap
      done
      '';
      serviceConfig = {
        User = "root";
        Group = "root";
        WorkingDirectory = "/tmp";
        PrivateTmp = true;
        Type = "forking";
      };
    };
  };
}
