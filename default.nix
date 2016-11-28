{ pkgs ? (import <nixpkgs> {}), ... }:

(import ./srk-nixpkgs/default.nix { 
    inherit pkgs; 
    inherit (import ./compileconfig.nix) genesisN slotDuration networkDiameter mpcRelayInterval;
}).cardano-sl
