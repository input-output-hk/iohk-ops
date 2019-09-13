{ lib, pkgs, config, ... }:
let
  inherit (lib) mkIf concatMapStringsSep mapAttrsFlatten filterAttrs concatStringsSep;

  guestIPs = lib.mapAttrsFlatten (k: v: let
    pfx = v.network.interiorNetworkPrefix;
  in {
    subnetIP = "${pfx}.0";
    routerIP = "${pfx}.1";
    guestIP = "${pfx}.2";
    broadcastIP = "${pfx}.255";
    sshPort = v.network.guestSshPort;
    prometheusPort = v.network.prometheusPort;
    MACAddress = v.guest.MACAddress;
    name = k;
  }) config.macosGuest.machines;
  makeHostsFor = tapDevice: let
    filteredGuests = filterAttrs (key: v: v.network.tapDevice == tapDevice) config.macosGuest.machines;
    hostBlocks = mapAttrsFlatten (k: v: ''
        host builder-${k} {
          hardware ethernet ${v.guest.MACAddress};
          fixed-address ${v.network.guestIP};
        }
    '') filteredGuests;
  in concatStringsSep "\n" hostBlocks;
  mkLogServer = port: {
    wantedBy = [ "multi-user.target" ];
    script = let
      ncl = pkgs.writeScript "ncl" ''
        #!/bin/sh
        set -euxo pipefail
        ${pkgs.netcat}/bin/nc -dklun ${toString port} | ${pkgs.coreutils}/bin/tr '<' $'\n'
      '';
    in ''
      set -euxo pipefail
      ${pkgs.expect}/bin/unbuffer ${ncl}
    '';
  };
in {
  config = mkIf config.macosGuest.enable {
    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
    boot.kernel.sysctl."net.ipv4.conf.default.forwarding" = true;

    networking.firewall.extraCommands = lib.concatMapStringsSep "\n" (v: ''
      ip46tables -A nixos-fw -i tap-${v.name} -p udp --dport 53 -j nixos-fw-accept # knot dns / kresd
    '') guestIPs;

    networking.firewall.allowedTCPPorts = [
      2200 # forwarded to :22 on the guest for external SSH
      2201
      9101 # forwarded to :9100 on the guest
      config.services.prometheus.exporters.node.port
    ];
    networking.firewall.allowedUDPPorts = [
      1514 # guest sends logs here
      1515
      1516
    ];

    networking.nat = {
      enable = true;
      externalInterface = config.macosGuest.network.externalInterface;
      internalInterfaces = map (v: "tap-${v.name}") guestIPs;
      internalIPs = map (x: "${x.guestIP}/24") guestIPs;
      forwardPorts = builtins.concatLists (map (x: [
        {
          destination = "${x.guestIP}:22";
          proto = "tcp";
          sourcePort = x.sshPort;
        }
        {
          destination = "${x.guestIP}:9100";
          proto = "tcp";
          sourcePort = x.prometheusPort;
        }
      ]) guestIPs);
    };

    networking.interfaces = lib.mapAttrs (k: v: {
        virtual = true;
        ipv4.addresses = [
          {
            address = "${v.subnet}.1";
            prefixLength = 24;
          }
        ];
      }) config.macosGuest.network.tapDevices;

    services.dhcpd4 = {
      enable = true;
      interfaces = map (v: "tap-${v.name}") guestIPs;
      extraConfig = ''
        authoritative;
        ${lib.concatStringsSep "\n" (mapAttrsFlatten (key: cfg: ''
          subnet ${cfg.subnet}.0 netmask 255.255.255.0 {
            option routers ${cfg.subnet}.1;
            option broadcast-address ${cfg.subnet}.255;
            option domain-name-servers ${cfg.subnet}.1;
            group {
          ${makeHostsFor key}
            }
          }
        '') config.macosGuest.network.tapDevices)}
      '';
    };

    services.kresd = {
      enable = true;
      interfaces = [ "::1" "127.0.0.1" ] ++ map (x: x.routerIP) guestIPs;
      extraConfig = ''
        modules = {
          'policy',   -- Block queries to local zones/bad sites
          'stats',    -- Track internal statistics
          'predict',  -- Prefetch expiring/frequent records
        }
        -- Smaller cache size
        cache.size = 10 * MB
      '';
    };

    services.prometheus.exporters.node = {
      enable = true;
    };

    systemd.services = {
      netcatsyslog-ci = mkLogServer 1514;
      netcatsyslog-signing = mkLogServer 1515;
    };
  };
}
