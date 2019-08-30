{ pkgs, ... }:
let
  pkgsUnstable = import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/362be9608c3.tar.gz") {
    config = {};
    overlays = [];
  };
  macBootOverlay = self: super {
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
