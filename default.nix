# ./build.sh

{pkgs, ...}:

(import ./srk-nixpkgs/default.nix { inherit pkgs; }).cardano-sl

