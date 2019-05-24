{ globals, ... }:

with import ../lib.nix;

let
  nodeMap = { inherit (globals.fullMap) explorer; };
in
  (flip mapAttrs nodeMap (name: value: ../modules/cardano-development.nix))
