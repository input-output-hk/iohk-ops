{ pkgs, config, lib, ... }:
let
  cfg = config.services.auto-gc;
in {
  options = {
    services.auto-gc = {
      nixAutoMaxFreedGB = lib.mkOption {
        type = lib.types.int;
        default = 110;
        description = "An absolute amount to free at";
      };

      nixAutoMinFreeGB = lib.mkOption {
        type = lib.types.int;
        default = 30;
        description = "The minimum amount to trigger a GC at";
      };

      nixAbsoluteTimedGB = lib.mkOption {
        type = lib.types.int;
        default = 25;
        description = "The max absolute level to free to on the /nix/store mount for the timed GC";
      };
    };
  };
  config = {
    nix = {
      gc = {
        automatic = true;
        dates = "*:15:00";

        # Set the max absolute level to free to nixAbsoluteTimedGB on the /nix/store mount
        options = ''--max-freed "$((${toString cfg.nixAbsoluteTimedGB} * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';
      };

      # This GC is run automatically by nix-build
      extraOptions = ''
        # Try to ensure between ${toString cfg.nixAutoMinFreeGB}G and ${toString cfg.nixAutoMaxFreedGB}G of free space by
        # automatically triggering a garbage collection if free
        # disk space drops below a certain level during a build.
        min-free = ${toString (cfg.nixAutoMinFreeGB * 1024 * 1024 * 1024)}
        max-free = ${toString (cfg.nixAutoMaxFreedGB * 1024 * 1024 * 1024)}
      '';
    };
  };
}
