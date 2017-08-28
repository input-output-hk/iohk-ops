{ ... }:

let
  node = {
    imports = [ ../modules/cardano-node-scaling.nix ];
  };
in {
  core-1 = node;
  core-2 = node;
  core-3 = node;
  relay-1 = node;
}
