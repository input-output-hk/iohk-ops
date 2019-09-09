# Module for a buildkite agent that mounts the NFS cache from the
# given server.

{ server }:

{ config, pkgs, ... }:

{
  fileSystems."/cache" = {
    device = "${server}:/srv/nfs/cache";
    fsType = "nfs";
    options = [ "soft" "x-systemd.automount" "noauto" ];
  };

  systemd.services."buildkite-agent.service" = {
    wants = [ "mnt-cache.mount" ];
  };
}
