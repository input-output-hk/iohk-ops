{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./generated.nix ];
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  services.openssh.enable = true;
  boot.zfs.devNodes = "/dev"; # fixes some virtualmachine issues
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;
  boot.kernelPatches = [ pkgs.kernelPatches.mac_nvme_t2 ];
  boot.kernelPackages = pkgs.linuxPackages_5_2;
  boot.kernelParams = [
    "boot.shell_on_fail"
    "panic=30" "boot.panic_on_fail" # reboot the machine upon fatal boot issues
  ];
  # hard-coded to devops that can provision macs
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC538FOusIV3UgbXukgtamrb7d6cCwVVczlXhxuqOT9HBasuydkYIyYD10bJogTUmW9BeuzAZRysmEMbG1stEEx178cIeBeAGx/H3DdA4kRJUUdRCx17KMcXUz2eSeToygM5pgQL0XqDqGIwOOlwNtfm2KOMS8oJtaKNu2WXLhmGN69OxslgtKMQG3HB0tdUMha2R2rKbQ2nNad6ldAlp+cwBIursJKtYZ7cCavMQG2ocxvd+dRKpbvPF2zwAegR8ysOBJizCVJq8aKyPRhnOrWvpeKTqU+miK9q/kZ/4s7cmkKGPnG/AWhZ2GRITm9NjuOJBA7sXwBOG6u+XXaJbLHqgV/zLiz7qSmeCOTtDRjZId98fZ4AMURNrj/74OsYi8PnHm1crpSjI7BJPEWwiJ/DaxptKOs7wpsbSqW+lzfhFw92tPDhK6AnOkUjVuW02gJSXcVjh5NkbWVC2Koy7rorNdpqCWlIVpDFW6LoECRfaLdEjXRbxxWvGwiQOn/jHbZoXaBQLMuXyMwRiLYt6LhuFHkS0E1d2Bikgx8DxaE33yJaMPaWlqlg6Dl28wKp1ZXxhflSzVs7D9CwqILrR7yiBTnSAO7IZ/JBFoVzcMXl3Y68n7Q3wf4D7+Ph9LJl/jQHZm/fC/tMIdWgmpyCC2gQ/ztDW2jOTlFwEymgrLd9w== iohk-key"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbtjKqNiaSVMBBSK4m97LXCBwhHMfJh9NBGBxDg+XYCLOHuuKw2MsHa4eMtRhnItELxhCAXZg0rdwZTlxLb6tzsVPXAVMrVniqfbG2qZpDPNElGdjkT0J5N1X1mOzmKymucJ1uHRDxTpalpg5d5wyGZgwVuXerep3nnv2xIIYcHWm5Hy/eG0pRw6XuQUeMc3xSU5ChqZPwWqhdILkYteKMgD8vxkfmYE9N/2fPgKRmujKQ5SA0HoJYKBXo29zGQY5r6nt3CzuIANFD3vG3323Znvt8dSRd1DiaZqKEDWSI43aZ9PX2whYEDyj+L/FvQa78Wn7pc8Nv2JOb7ur9io9t michael.bishop"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5PPVdfBhmWxWYjSosuyjMdIYNjYC/ekz+Whv27wrFNHqxeGgKbXslUTwZX0r+zu/nlJnX9nj3zdVV9LosBB8JF9tfJGui9aBfPuxoIq9SMFSdcpZ8aOh4ZITv7zbsRHMECE8q7D5/a+7UZyTy8pv9g5SuCerHh3m//NIbo08OS9rt8SjqVio+B+rseLF960U3U3wTCtOA+VauTuE4kZfSfmQlEYUjaN3qwp4s5jpO7pgnGxshuqayRyuwJfRa/RYWB5ouSjyxTuo33K42EqT4XFoURkj7evJB5SRR7pm4vJCx4VkclIVmpLIcBiyWje+60zyKhAZEQVqKXedkuQ9748wZl07C6Czs4QiloGAjXv/tRm9YSdoeG5JhskEA8z2SCEQARJGquPH+f5vBltHeVC5K5LW94gSP9bfVBitcCgONVxUguCu0PmJUYKcVVjRi3KtJJzDSTDCjjN3e/mszrZY921yvVEkb7mFATBiHeSdrt55gKcG1vfTToLALIJJFQpGCwAMYUjKEcgq4PZa1UdCY/ynvynLds3mge4Y/X3EnLFsJaepfgNyPnnPg67kEda8uRSDYT8LaoqJpDzc7RQeY4BOfJfAxa8qMDHmp4W+dxHqrMphbH66fwUJAx1MWV8AoPFW0TGrDb3AnHBgoRt/5Fnz2ymy92Wb1KAIt3w== sam@optina"
  ];
}
