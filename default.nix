# nix-build -E 'with import ../nixpkgs {}; callPackage ./defaultRscoin.nix { }'

{pkgs, ...}:

(import ./srk-pkgs.nix { inherit pkgs; }).cardanod;

