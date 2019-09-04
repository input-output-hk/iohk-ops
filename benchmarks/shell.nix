{ commonLib ? import ../lib.nix
, pkgs ? commonLib.pkgs
, io-dev-mode ? false
}:

let
  iohk-ops = (import ./default.nix {}).iohk-ops;
  ioAlias  = if io-dev-mode
             then ''runhaskell -iiohk ../iohk/iohk-ops.hs''
             else ''${iohk-ops}/bin/iohk-ops'';
in pkgs.lib.overrideDerivation iohk-ops.env
   (old: {
     shellHook = ''
       function io {
        ${ioAlias} "$@"
       }
     '';
    })
