{ localLib ? import ./lib.nix
, pkgs ? import (localLib.fetchNixPkgs) {}
}:

let
  extraDeps = with pkgs;
    [ wget awscli nodejs # for scripts/aws.hs
    ];
  drv = pkgs.haskell.lib.overrideCabal
         (import ./default.nix {}).iohk-ops
         (drv: { libraryHaskellDepends = extraDeps; });
in drv.env
