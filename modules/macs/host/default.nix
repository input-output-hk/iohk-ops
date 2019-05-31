{ lib, config, ... }:

let
  inherit (lib) mkOption types mkDefault;
  guestOptions = { name, config, ... }: {
    options = {
      zvolName = mkOption {
        type = types.str;
        description = ''
          Name of the zvol containing the root disk image.
        '';
        example = "rpool/my-disk-image";
      };

      guest = {
        guestConfigDir = mkOption {
          type = types.path;
          description = ''
            A directory of configuration files to expose to the VM.

            At a minimum, it should contain an `apply.sh` file in the
            root. This is executed by the macOS VM on boot-up. Note
            the configuration will be mounted at /Volumes/CONFIG as a
            cdrom.

            At /Volumes/CONFIG/etc/ssh/ will be SSH host keys which
            should be copied to /etc/ssh/ on the host. Additionally,
            the script should finish by unmounting /Volumes/CONFIG
            otherwise it is possible for programs runnig on the guest
            to read the SSH host keys.
          '';
        };
        persistentConfigDir = mkOption {
          type = types.str;
          description = ''
            A path on the guest to store secret, persistent
            configuration like SSH host keys.

            Host keys are generated on the host and copied to the VM
            to ensure they don't change on every boot.
          '';
        };
        MACAddress = mkOption {
          type = types.str;
          description = "The MAC address to assign the guest's NIC.";
          default = "52:54:00:c9:18:27";
        };
        cloverImage = mkOption {
          type = types.path;
          description = "Path to the Clover bootloader.";
        };
        sockets = mkOption {
          type = types.int;
          description = ''
            The number of physical CPU Sockets in the system.

              # lscpu
              Architecture:        x86_64
              CPU op-mode(s):      32-bit, 64-bit
              Byte Order:          Little Endian
              CPU(s):              4
              On-line CPU(s) list: 0-3
              Thread(s) per core:  2
              Core(s) per socket:  2
              Socket(s):           1      <------
          '';
        };
        cores = mkOption {
          type = types.int;
          description = ''
            The number of Cores per Socket.

              # lscpu
              Architecture:        x86_64
              CPU op-mode(s):      32-bit, 64-bit
              Byte Order:          Little Endian
              CPU(s):              4
              On-line CPU(s) list: 0-3
              Thread(s) per core:  2
              Core(s) per socket:  2      <------
              Socket(s):           1
          '';
        };
        threads = mkOption {
          type = types.int;
          description = ''
            The number of Threads per Core.

              # lscpu
              Architecture:        x86_64
              CPU op-mode(s):      32-bit, 64-bit
              Byte Order:          Little Endian
              CPU(s):              4
              On-line CPU(s) list: 0-3
              Thread(s) per core:  2      <------
              Core(s) per socket:  2
              Socket(s):           1
          '';
        };
        memoryInMegs = mkOption {
          type = types.int;
          description = ''
            I have no idea what "megs" is, but QEMU's documentatation
            says this is the number of megs. Save 1G or 2G or so for
            the host and ZFS.
          '';
        };
        ovmfCodeFile = mkOption {
          type = types.path;
          description = "Path to the OVMF Code File.";
        };
        ovmfVarsFile = mkOption {
          type = types.path;
          description = "Path to the OVMF Variable File.";
        };
        vncListen = mkOption {
          type = types.str;
          example = "192.168.20.20:0";
        };
      };
      network = {
        guestSshPort = mkOption {
          type = types.int;
          example = 2200;
        };
        prometheusPort = mkOption {
          type = types.int;
          example = 9101;
        };
        interiorNetworkPrefix = mkOption {
          type = types.str;
          description = ''
            The first three octets of the network to use for the virtual
            machine. The VM always runs in a /24 network. If you use the
            value "192.168.1", the host will have IP 192.168.1.1 and the
            guest will have IP 192.168.1.2
          '';

          example = "192.168.1";
        };
        tapDevice = mkOption {
          type = types.str;
        };
        guestIP = mkOption {
          type = types.str;
        };
      };
    };
    config = {
      guest.persistentConfigDir = mkDefault "/var/lib/macos-vm-persistent-config-${name}";
    };
  };
  tapOptions = { name, config, ... }: {
    options = {
      subnet = mkOption {
        type = types.str;
        example = "192.168.3";
        description = "the prefix for all IP's on this tap device";
      };
    };
  };
in {
  options = {
    monitorama = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Whether to enable a prometheus proxy for prom nodes behind
          a NAT.
        '';
      };

      hosts = mkOption {
        type = types.attrsOf types.str;
        description = ''
          Key, value pairs of name -> ip:port/paths. The name will be
          used in a proxy's path.
        '';
        example = {
          "/mac1/host" = "http://192.168.2.101:9100/metrics";
          "/mac1/guest" = "http://192.168.2.101:9101/metrics";
        };
      };
    };
    macosGuest = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Whether to enable the macOS guest, including networking and
          the QEMU VM.
        '';
      };

      network = {
        externalInterface = mkOption {
          type = types.str;
          description = ''
            Public network interface to forward traffic through.
          '';
        };
        tapDevices = mkOption {
          type = types.loaOf (types.submodule tapOptions);
          default = {};
        };
      };
      machines = mkOption {
        type = types.loaOf (types.submodule guestOptions);
        default = {};
      };

      guest = {
        snapshotName = mkOption {
          type = types.str;
          description = ''
            Name of the snapshot on the zvolName.

            There must be a snapshot because the disk state is rolled
            back on every boot.

            The snapshot name is combined with zvolName like:
            zvolName@snapshotName
          '';
          example = "pristine";
          default = "pristine";
        };
      };
    };
  };

  imports = [
    ./networking.nix
    ./qemu.nix
    ./monitorama.nix
  ];
}
