{ pkgs, ... }:
let
  # TODO: switch to 19.03 if this PR gets merged before freeze
  pkgsUnstable = import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/a8d26fc34a7ceb52d098e384f316b8e4b19651bb.tar.gz") {
    config = {};
    overlays = [];
  };
  macBootOverlay = self: super: {
    inherit (pkgsUnstable) grub2;
  };
in {
  nixpkgs = {
    overlays = [
      macBootOverlay
    ];
  };
  boot.kernelPatches = [ pkgsUnstable.kernelPatches.mac_nvme_t2 ];
  boot.kernelPackages = pkgsUnstable.linuxPackages_5_2;
}
