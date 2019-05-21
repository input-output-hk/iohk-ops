with import ./../lib.nix;
let
  eval = import <nixpkgs/nixos> {
    configuration = cfg;
  };
  cfg = { ... }: {
    imports = [
      ./monitoring-services.nix
      <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
    ];
    virtualisation.memorySize = 4 * 1024;
    virtualisation.graphics = false;
    services.mingetty.autologinUser = "root";

    virtualisation.qemu.networkingOptions = [
      "-net nic,netdev=user.0,model=virtio"
      "-netdev user,id=user.0,hostfwd=tcp:127.0.0.1:8080-:80"
    ];
    services.monitoring-services = {
      enable = true;
      enableACME = false;
      webhost = "localhost";
      grafanaCreds = makeCreds "grafana" { user = "changeme"; password = "changeme"; };
      graylogCreds = makeCreds "graylog" { user = "changeme"; password = "changeme"; };
      oauth = {
        enable = false;
      };
    };
  };
in {
  vm = eval.vm;
  config = eval.config;
}
