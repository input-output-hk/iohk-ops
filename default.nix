{ pkgs ? (import <nixpkgs> {}), ... }:

(import ./srk-nixpkgs/default.nix { 
    inherit pkgs; 
    inherit (import ./config.nix) genesisN slotDuration networkDiameter mpcRelayInterval;
}).cardano-sl
