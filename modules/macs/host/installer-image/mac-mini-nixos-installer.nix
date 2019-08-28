{
  boot.supportedFilesystems = [ "zfs" ];
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    ../macmini-boot-fixes.nix
    ./justdoit.nix
  ];
  kexec.justdoit = {
    rootDevice = "/dev/nvme0n1";
    bootSize = 1024;
    bootType = "vfat";
    swapSize = 1024 * 16;
    poolName = "tank";
    uefi = true;
    nvme = true;
  };
}
