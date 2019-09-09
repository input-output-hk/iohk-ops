
{ config, lib, pkgs, ... }:
let
  inherit (config.macosGuest.guest) threads cores sockets memoryInMegs
    ovmfCodeFile ovmfVarsFile cloverImage snapshotName;
  inherit (lib) mkIf;

in {
  config = mkIf config.macosGuest.enable {

    systemd.services = builtins.listToAttrs (builtins.concatLists (lib.mapAttrsFlatten (key: value: let
      inherit (value) zvolName;
      snapshot = "${zvolName}@${snapshotName}";
      clonedVol = "${zvolName}-${key}";
      clonedZvolDevice = "/dev/zvol/${clonedVol}";
      inherit (value.guest) cores threads sockets memoryInMegs ovmfCodeFile ovmfVarsFile cloverImage MACAddress;
      deps = [ "create-macos-secrets-${key}.service" "dhcpd4.service" "kresd.service" "network-online.target" ];
    in [ {
      name = "run-macos-vm-${key}";
      value = {
        requires = deps;
        after = deps;
        wantedBy = [ "multi-user.target" ];
        wants = [ "netcatsyslog.service" ];
        path = with pkgs; [ zfs qemu cdrkit rsync findutils ];

        serviceConfig.PrivateTmp = true;

        preStart = ''
          echo prestart script
          zfs destroy ${clonedVol} || true
          while [ -e /dev/${clonedVol} ]
          do
            echo "waiting for volume to finish removing"
            sleep 5
          done
          zfs clone ${snapshot} ${clonedVol}

          # Create a cloud-init style cdrom
          rm -rf /tmp/cdr
          cp -r ${value.guest.persistentConfigDir} /tmp/cdr
          rsync -a ${value.guest.guestConfigDir}/ /tmp/cdr
          cd /tmp/cdr
          find .
          genisoimage -v -J -r -V CONFIG -o /tmp/config.iso .
        '';
        postStop = ''
          echo poststop script
          while [ -e /dev/${clonedVol} ]
          do
            zfs destroy ${clonedVol} || (echo "waiting for volume to finish removing" ; sleep 1)
          done
        '';
        script = ''
          echo main script
          exec qemu-system-x86_64 \
              -enable-kvm \
              -cpu Penryn,kvm=on,vendor=GenuineIntel,+invtsc,vmware-cpuid-freq=on,+aes,+xsave,+avx,+xsaveopt,avx2,+smep \
              -machine pc-q35-2.9 \
              -smp cpus=${toString (cores * threads * sockets)},cores=${toString cores},threads=${toString threads},sockets=${toString sockets} \
              -m ${toString memoryInMegs} \
              -usb -device usb-kbd -device usb-tablet \
              -device isa-applesmc,osk="ourhardworkbythesewordsguardedpleasedontsteal(c)AppleComputerInc" \
              -drive if=pflash,format=raw,readonly,file=${ovmfCodeFile} \
              -drive if=pflash,format=raw,snapshot=on,file=${ovmfVarsFile} \
              -smbios type=2 \
              -device ich9-intel-hda -device hda-duplex \
              -device ide-drive,bus=ide.2,drive=Clover \
              -drive id=Clover,if=none,snapshot=on,format=qcow2,file='${cloverImage}' \
              -device ide-drive,bus=ide.1,drive=MacHDD \
              -drive id=MacHDD,cache=unsafe,if=none,file=${clonedZvolDevice},format=raw \
              -device ide-drive,bus=ide.0,drive=config \
              -drive id=config,if=none,snapshot=on,media=cdrom,file=/tmp/config.iso \
              -netdev tap,id=net0,ifname=tap-${key},script=no,downscript=no -device e1000-82545em,netdev=net0,id=net0,mac=${MACAddress} \
              -global PIIX4_PM.disable_s3=1 -global PIIX4_PM.disable_s5=1 \
              -vnc ${value.guest.vncListen} \
              -monitor unix:/tmp/monitor-socket,server,nowait
        '';
      };
    }
    {
      name = "create-macos-secrets-${key}";
      value = {
        path = with pkgs; [ openssh ];

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };

        script = ''
          if [ ! -f ${value.guest.persistentConfigDir}/etc/ssh/ssh_host_ed25519_key ]; then
            mkdir -p ${value.guest.persistentConfigDir}/etc/ssh
            ssh-keygen -A -f ${value.guest.persistentConfigDir}
          fi
        '';
      };
    } ]) config.macosGuest.machines));
  };
}
