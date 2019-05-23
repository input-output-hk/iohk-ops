let
  pkgsSrc = import ../fetch-nixpkgs.nix;
  eval = import (pkgsSrc + "/nixos") {
    configuration = cfg;
  };
  cfg = { lib, ... }: {
    imports = [
      ./hydra-master-main.nix
      ./common.nix
      ./hydra-slave.nix
      ./hydra-master-common.nix
    ];
    virtualisation = {
      memorySize = 2 * 1024;
      graphics = false;
      qemu.networkingOptions = [
        "-net nic,netdev=user.0,model=virtio"
        "-netdev user,id=user.0,hostfwd=tcp:127.0.0.1:8080-:80"
      ];
    };
    services = {
      mingetty.autologinUser = "root";
      grafana.extraOptions.AUTH_GOOGLE_CLIENT_SECRET = lib.mkForce "";
      nginx.virtualHosts."hydra.iohk.io" = {
        forceSSL = lib.mkForce false;
        enableACME = lib.mkForce false;
      };
    };
    nixpkgs.overlays = [
      (import ../overlays/monitoring-exporters.nix)
    ];
  };
in eval.vm
