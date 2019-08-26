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
        "-netdev user,id=user.0,hostfwd=tcp:127.0.0.1:8000-:8000"
      ];
    };
    networking.firewall.allowedTCPPorts = [ 8000 ];
    services = {
      mingetty.autologinUser = "root";
      grafana.extraOptions.AUTH_GOOGLE_CLIENT_SECRET = lib.mkForce "";
      hydra.hydraURL = lib.mkForce "http://localhost:8000";
      monitoring-exporters.logging = false;
      nginx.virtualHosts."hydra.iohk.io" = {
        forceSSL = lib.mkForce false;
        enableACME = lib.mkForce false;
        listen = lib.mkForce [ { addr = "0.0.0.0"; port = 8000; } ];
      };
    };
  };
in eval.vm
