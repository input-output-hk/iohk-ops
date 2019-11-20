{ pkgs, ... }:
let
  nixAutoMaxFreedMB = 20000;     # An absolute amount to free
  nixAutoMinFreeMB  = 10000;     # The min to trigger a GC at
in {
  nix = {
    gc = {
      automatic = true;
      dates = "*:15:00";

      # Set the max absolute level to free to 30 GB on the /nix/store mount
      options = ''--max-freed "$((30 * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';
    };

    # This GC is run automatically by nix-build
    extraOptions = ''
      # Try to ensure between ${toString nixAutoMinFreeMB}M and ${toString nixAutoMaxFreedMB}M of free space by
      # automatically triggering a garbage collection if free
      # disk space drops below a certain level during a build.
      min-free = ${toString (nixAutoMinFreeMB * 1048576)}
      max-free = ${toString (nixAutoMaxFreedMB * 1048576)}
    '';
  };
}
