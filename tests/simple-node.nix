let
  nodeMap = import ./cardano-node-simple-config.nix;
in
import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }: {
  name = "simple-node";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [ (import ../modules/cardano.nix (nodeMap.machine)) ];
      virtualisation.qemu.options = [ "-cpu Haswell" ];
      services.cardano-node = {
        autoStart = true;
        neighbours = [];
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("cardano-node.service");
    # TODO, implement sd_notify?
    $machine->waitForOpenPort(3000);
  '';
})
