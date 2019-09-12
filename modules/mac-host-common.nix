{ pkgs, lib, config, ... }:

let
  cfg = config.macosGuest;
in {
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ./macs/host
    ./macs/host/macmini-boot-fixes.nix
    ./cachecache.nix
  ];
  options = {
    #macosGuest.role = lib.mkOption {
    #  type = lib.types.enum [ "buildkite-agent" "hydra-slave" ];
    #};
  };
  config = {
    boot = {
      initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "zfsUnstable" "nvme" ];
      kernelModules = [ "kvm-intel" ];
      extraModulePackages = with config.boot.kernelPackages; lib.mkForce [ zfsUnstable wireguard ];
      loader = {
        efi.canTouchEfiVariables = false;
        grub = {
          enable = true;
          version = 2;
          efiInstallAsRemovable = true;
          efiSupport = true;
          device = "nodev";
        };
      };
    };
    nix.maxJobs = 4;
    nixpkgs = {
      config.allowUnfree = true;
    };
    networking.firewall.allowedTCPPorts = [ 5900 5901 8081 ];
    networking.firewall.extraCommands = lib.mkAfter ''
      iptables -t nat -A nixos-nat-pre -i wg0 -p tcp -m tcp --dport 2200 -j DNAT --to-destination 192.168.3.2:22
      iptables -t nat -A nixos-nat-pre -i wg0 -p tcp -m tcp --dport 2201 -j DNAT --to-destination 192.168.4.2:22
    '';
    networking.wireguard.interfaces.wg0 = let
      genPeer = n: name: endpoint: {
        publicKey = lib.strings.removeSuffix "\n" (builtins.readFile (../static + "/${name}.wgpublic"));
        allowedIPs = [ "192.168.20.${toString n}/32" ];
        persistentKeepalive = 30;
        inherit endpoint;
      };
    in {
      listenPort = 51820;
      privateKeyFile = "/etc/wireguard/private.key";
      peers = [
        {
          publicKey = "kf/f+PWsMPVtV0vMvjG7A8ShgRfdwFAb99u+ixBboBE=";
          allowedIPs = [ "192.168.20.0/24" ];
          endpoint = "monitoring.aws.iohkdev.io:51820";
          persistentKeepalive = 30;
        }
        {
          publicKey = "oycbQ1DhtRh0hhD5gpyiKTUh0USkAwbjMer6/h/aHg8=";
          allowedIPs = [ "192.168.21.1/32" ];
          endpoint = "99.192.62.202:51820";
          persistentKeepalive = 30;
        }
        (genPeer 2 "hydra" "hydra.iohk.io:51820")
        (genPeer 3 "cardano-deployer" "${lib.strings.removeSuffix "\n" (builtins.readFile ../static/deployer-ip.txt)}:51820")
        {
          publicKey = "MRowDI1eC9B5Hx/zgPk5yyq2eWSq6kYFW5Sjm7w52AY=";
          allowedIPs = [ "192.168.24.1/32" ];
          persistentKeepalive = 30;
          endpoint = "173.61.28.54:51820";
        }
      ];
    };
    monitorama = {
      enable = true;
      hosts = {
        "/monitorama/host" = "http://127.0.0.1:9100/metrics";
        "/monitorama/ci" = "http://192.168.3.2:9100/metrics";
        "/monitorama/signing" = "http://192.168.4.2:9100/metrics";
      };
    };
    environment.systemPackages = with pkgs; [
      wget vim screen
    ];
    users.users.root = {
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbtjKqNiaSVMBBSK4m97LXCBwhHMfJh9NBGBxDg+XYCLOHuuKw2MsHa4eMtRhnItELxhCAXZg0rdwZTlxLb6tzsVPXAVMrVniqfbG2qZpDPNElGdjkT0J5N1X1mOzmKymucJ1uHRDxTpalpg5d5wyGZgwVuXerep3nnv2xIIYcHWm5Hy/eG0pRw6XuQUeMc3xSU5ChqZPwWqhdILkYteKMgD8vxkfmYE9N/2fPgKRmujKQ5SA0HoJYKBXo29zGQY5r6nt3CzuIANFD3vG3323Znvt8dSRd1DiaZqKEDWSI43aZ9PX2whYEDyj+L/FvQa78Wn7pc8Nv2JOb7ur9io9t michael.bishop"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5PPVdfBhmWxWYjSosuyjMdIYNjYC/ekz+Whv27wrFNHqxeGgKbXslUTwZX0r+zu/nlJnX9nj3zdVV9LosBB8JF9tfJGui9aBfPuxoIq9SMFSdcpZ8aOh4ZITv7zbsRHMECE8q7D5/a+7UZyTy8pv9g5SuCerHh3m//NIbo08OS9rt8SjqVio+B+rseLF960U3U3wTCtOA+VauTuE4kZfSfmQlEYUjaN3qwp4s5jpO7pgnGxshuqayRyuwJfRa/RYWB5ouSjyxTuo33K42EqT4XFoURkj7evJB5SRR7pm4vJCx4VkclIVmpLIcBiyWje+60zyKhAZEQVqKXedkuQ9748wZl07C6Czs4QiloGAjXv/tRm9YSdoeG5JhskEA8z2SCEQARJGquPH+f5vBltHeVC5K5LW94gSP9bfVBitcCgONVxUguCu0PmJUYKcVVjRi3KtJJzDSTDCjjN3e/mszrZY921yvVEkb7mFATBiHeSdrt55gKcG1vfTToLALIJJFQpGCwAMYUjKEcgq4PZa1UdCY/ynvynLds3mge4Y/X3EnLFsJaepfgNyPnnPg67kEda8uRSDYT8LaoqJpDzc7RQeY4BOfJfAxa8qMDHmp4W+dxHqrMphbH66fwUJAx1MWV8AoPFW0TGrDb3AnHBgoRt/5Fnz2ymy92Wb1KAIt3w== sam@optina"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC538FOusIV3UgbXukgtamrb7d6cCwVVczlXhxuqOT9HBasuydkYIyYD10bJogTUmW9BeuzAZRysmEMbG1stEEx178cIeBeAGx/H3DdA4kRJUUdRCx17KMcXUz2eSeToygM5pgQL0XqDqGIwOOlwNtfm2KOMS8oJtaKNu2WXLhmGN69OxslgtKMQG3HB0tdUMha2R2rKbQ2nNad6ldAlp+cwBIursJKtYZ7cCavMQG2ocxvd+dRKpbvPF2zwAegR8ysOBJizCVJq8aKyPRhnOrWvpeKTqU+miK9q/kZ/4s7cmkKGPnG/AWhZ2GRITm9NjuOJBA7sXwBOG6u+XXaJbLHqgV/zLiz7qSmeCOTtDRjZId98fZ4AMURNrj/74OsYi8PnHm1crpSjI7BJPEWwiJ/DaxptKOs7wpsbSqW+lzfhFw92tPDhK6AnOkUjVuW02gJSXcVjh5NkbWVC2Koy7rorNdpqCWlIVpDFW6LoECRfaLdEjXRbxxWvGwiQOn/jHbZoXaBQLMuXyMwRiLYt6LhuFHkS0E1d2Bikgx8DxaE33yJaMPaWlqlg6Dl28wKp1ZXxhflSzVs7D9CwqILrR7yiBTnSAO7IZ/JBFoVzcMXl3Y68n7Q3wf4D7+Ph9LJl/jQHZm/fC/tMIdWgmpyCC2gQ/ztDW2jOTlFwEymgrLd9w== iohk-key"
      ];
    };
    swapDevices = [
      {
        label = "swap";
      }
    ];
    services = {
      pcscd.enable = true;
      cachecache.enable = true; # port 8081
      openssh = {
        enable = true;
        permitRootLogin = "yes";
      };
    };
    powerManagement.cpuFreqGovernor = "performance";
    fileSystems = {
      "/" = {
        fsType = "zfs";
        device = "tank/root";
      };
      "/home" = {
        fsType = "zfs";
        device = "tank/home";
      };
      "/nix" = {
        fsType = "zfs";
        device = "tank/nix";
      };
      "/boot" = {
        label = "EFI";
        fsType = "vfat";
      };
    };
    macosGuest = let
      guestConfDir1 = host: port: hostname: (import ../nix-darwin/test.nix { role = "ci"; inherit host port hostname; }).guestConfDir;
      guestConfDir2 = host: port: hostname: (import ../nix-darwin/test.nix { role = "signing"; inherit host port hostname; }).guestConfDir;
    in {
      enable = true;
      network = {
        externalInterface = "ens1";
        tapDevices = {
          tap-ci = {
            subnet = "192.168.3";
          };
          tap-signing = {
            subnet = "192.168.4";
          };
        };
      };
      machines = {
        ci = {
          zvolName = "tank/mojave-image1";
          network = {
            interiorNetworkPrefix = "192.168.3";
            guestSshPort = 2200;
            prometheusPort = 9101;
            tapDevice = "tap-ci";
            guestIP = "192.168.3.2";
          };
          guest = {
            guestConfigDir = guestConfDir1 "192.168.3.1" "1514" "${config.networking.hostName}-ci";
            cores = 2;
            threads = 2;
            sockets = 1;
            memoryInMegs = 6 * 1024;
            ovmfCodeFile = ./macs/dist/OVMF_CODE.fd;
            ovmfVarsFile = ./macs/dist/OVMF_VARS-1024x768.fd;
            cloverImage = (pkgs.callPackage ./macs/clover-image.nix { csrFlag = "0x23"; }).clover-image;
            MACAddress = "52:54:00:c9:18:27";
            vncListen = "0.0.0.0:0";
          };
        };
        signing = {
          zvolName = "tank/mojave-image2-xcode";
          network = {
            interiorNetworkPrefix = "192.168.4";
            guestSshPort = 2201;
            prometheusPort = 9102;
            tapDevice = "tap-signing";
            guestIP = "192.168.4.2";
          };
          guest = {
            guestConfigDir = guestConfDir2 "192.168.4.1" "1515" "${config.networking.hostName}-signing";
            cores = 2;
            threads = 2;
            sockets = 1;
            memoryInMegs = 6 * 1024;
            ovmfCodeFile = ./macs/dist/OVMF_CODE.fd;
            ovmfVarsFile = ./macs/dist/OVMF_VARS-1024x768.fd;
            cloverImage = (pkgs.callPackage ./macs/clover-image.nix { csrFlag = "0x23"; }).clover-image;
            MACAddress = "52:54:00:c9:18:28";
            vncListen = "0.0.0.0:1";
          };
        };
      };
    };
  };
}
