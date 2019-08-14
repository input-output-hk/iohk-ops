{ pkgs, ... }:
{
  boot.kernelPatches = [ pkgs.kernelPatches.mac_nvme_t2 ];
  boot.kernelPackages = pkgs.linuxPackages_5_2;
}
