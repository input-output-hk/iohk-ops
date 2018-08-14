{ globals, ... }: with (import ./../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in

(flip mapAttrs nodeMap (name: import ./../modules/cardano-development.nix))
