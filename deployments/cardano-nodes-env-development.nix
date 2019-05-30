{ globals, ... }:

with import ../lib.nix;

(flip mapAttrs globals.nodeMap (name: value: ../modules/cardano-development.nix))
// {
  network.description = "Cardano Development";
  require = [
    ./security-groups/allow-all.nix
  ];

}
