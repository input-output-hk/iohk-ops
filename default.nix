# nix-build -E 'with import ~/nixpkgs {}; callPackage ./default.nix { }'

{pkgs, ...}:

(import ./srk-nixpkgs/srk-pkgs.nix { inherit pkgs; }).cardano

