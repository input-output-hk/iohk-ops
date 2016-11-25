{ pkgs ? (import <nixpkgs> {}), ... }:

# TODO share this parameters with nixops.nix
let cconf = import ./compileconfig.nix;
in
(import ./srk-nixpkgs/default.nix { 
    inherit pkgs; 
    inherit (cconf) genesisN slotDuration networkDiameter mpcRelayInterval;
}).cardano-sl
