{ globals, ... }: with (import ./../lib.nix);

let nodeMap = { inherit (globals.fullMap) explorer; }; in

flip mapAttrs nodeMap
(name: import ./../modules/cardano-node-config.nix
       globals
       [./../modules/cardano-explorer.nix])
