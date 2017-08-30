let
  localLib = import ./lib.nix;
  nixpkgs  = localLib.fetchNixPkgs;
  pkgs     = import nixpkgs {};
  hpkgs    = pkgs.haskell.packages.ghc802;
in

{ ... }:
let
  drv     = hpkgs.callPackage ./iohk/default.nix {};
  drv'    = pkgs.haskell.lib.overrideCabal
            (import ./default.nix {}).iohk-ops
            (old: {
              libraryHaskellDepends = with pkgs;
                 [ cabal-install stack haskellPackages.intero
                   # scripts/aws.hs dependencies:
                   wget awscli
                 ];
             });
  drv''   = pkgs.lib.overrideDerivation
            drv'.env
            (old: {
              shellHook = ''
                export NIX_PATH=nixpkgs=${nixpkgs}
                export NIX_PATH_LOCKED=1
                echo   NIX_PATH LOCKED and set to $NIX_PATH >&2
              '';
             });
in
  drv''
