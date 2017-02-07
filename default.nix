{ pkgs ? (import <nixpkgs> {}), ... }:

(import ./srk-nixpkgs.nix { 
    inherit pkgs; 
    inherit (import ./config.nix) genesisN slotDuration networkDiameter mpcRelayInterval;
}).hspkgs.cardano-sl
