{ localLib ? import ./lib.nix
, pkgs ? import (localLib.fetchNixPkgs) {}
}:

let
  extraDeps = with pkgs;
    [ cabal-install stack haskellPackages.intero
      # scripts/aws.hs dependencies:
      wget awscli
    ];
  drv = pkgs.haskell.lib.overrideCabal
         (import ./default.nix {}).iohk-ops
         (drv: { libraryHaskellDepends = extraDeps; });
in drv.env
