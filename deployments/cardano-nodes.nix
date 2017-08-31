{ # nixops, network, args,
  globals, ... }: with (import ./../lib.nix);

flip mapAttrs globals.nodeMap
(name: import ./../modules/cardano-node-config.nix
       globals
       [])
