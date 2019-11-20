{ config, lib, pkgs, ... }:

let
  nixAutoMaxFreedMB = 33000;     # An absolute amount to free
  nixAutoMinFreeMB = 14000;
  maxFreedMB = 25000;            # A relative amount to free
  minFreeMB = 15000;

in {
  imports = [ ../services/builder-gc.nix ];

  # This GC is run automatically by nix-build
  nix.extraOptions = ''
    # Try to ensure between ${toString nixAutoMinFreeMB}M and ${toString nixAutoMaxFreedMB}M of free space by
    # automatically triggering a garbage collection if free
    # disk space drops below a certain level during a build.
    min-free = ${toString (nixAutoMinFreeMB * 1048576)}
    max-free = ${toString (nixAutoMaxFreedMB * 1048576)}
  '';

  # This GC is run on 15 minute intervals
  nix.builder-gc = {
    enable = true;
    inherit maxFreedMB minFreeMB;
  };
  # TODO, switch over?
  #nix.gc.automatic = true;
  #nix.gc.options = let
  #    gbFree = 25;
  #in "--max-freed $((${toString gbFree} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | awk '{ print $4 }')))";
}
