{ pkgs ? (import <nixpkgs> {}), ... }:

(import ./srk-nixpkgs/default.nix { inherit pkgs; }).cardano-sl
