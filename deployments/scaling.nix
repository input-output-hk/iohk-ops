{ ... }:

let
  node = {
    imports = [ ../modules/cardano-node-scaling.nix ];
  };
in {
  c-a-3 = node;
  c-a-2 = node;
  c-a-1 = node;
  c-b-1 = node;
  c-b-2 = node;
  c-c-1 = node;
  c-c-2 = node;
  r-a-1 = node;
  r-a-2 = node;
  r-b-1 = node;
  r-a-3 = node;
  r-b-2 = node;
  r-c-2 = node;
  r-c-1 = node;
}
