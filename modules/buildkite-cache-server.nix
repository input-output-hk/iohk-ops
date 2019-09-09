# Module for a buildkite agent that NFS exports its cache to the given
# list of clients.

{ clients ? [] }:

{ name, config, pkgs, lib, ... }:

let
  mkRulesHost = host: lib.concatMapStrings (mkRulesPort host) [4000 4001 4002];
  mkRulesPort = host: port: lib.concatMapStrings (proto: ''
    iptables -A INPUT -s ${host} -m state --state NEW -p ${proto} --dport ${toString port} -j ACCEPT
  '') ["tcp" "udp"];

in {
  systemd.services.cache-dir = {
    wantedBy = [ "nfs.service" "mnt-cache.mount" ];
    before = [ "nfs.service" "mnt-cache.mount" ];
    script = ''
      mkdir -p /srv/nfs/cache
      chown buildkite-agent:nogroup /srv/nfs/cache
    '';
    serviceConfig.Type = "oneshot";
  };

  fileSystems."/cache" = {
    device = "/srv/nfs/cache";
    fsType = "bind";
  };

  services.nfs = {
    enable = true;
    exports = ''
      /srv/nfs/cache ${lib.concatMapStringsSep " " (h: "${h}(insecure,rw,root_squash,no_subtree_check)") clients}
    '';
    statdPort = 4000;
    lockdPort = 4001;
    mountdPort = 4002;
    # hostName = name _ "-internal";
  };

  networking.firewall.extraCommands = lib.concatMapStrings mkRulesHost clients;
}
