let
  localLib = import ./lib.nix;
  nixpkgs  = localLib.fetchNixPkgs;
  pkgs     = import nixpkgs {};
  hpkgs    = pkgs.haskell.packages.ghc802;
in

{ ... }:
let
  drv     = hpkgs.callPackage ./pkgs/iohk-ops.nix {};
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                 [ pkgs.cabal-install pkgs.stack pkgs.haskell.packages.ghc802.intero ];
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
