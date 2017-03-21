{ pkgs ? (import <nixpkgs> {}), ... }:

(import ./srk-nixpkgs.nix { inherit pkgs;}).hspkgs.cardano-sl-static
