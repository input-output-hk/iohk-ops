{ localLib ? import ./lib.nix
, pkgs ? import (localLib.fetchNixPkgs) {}
, io-dev-mode ? false
}:

let
  iohk-ops = (import ./default.nix {}).iohk-ops;
  ioAlias  = if io-dev-mode
             then ''runhaskell -iiohk iohk/iohk-ops.hs''
             else ''${iohk-ops}/bin/iohk-ops'';
  ioCompletion = if io-dev-mode then ""
                 else let iohk = "${iohk-ops}/bin/iohk-ops";
                 in ''
                   source <(${iohk} --bash-completion-script ${iohk})
                   complete -o filenames -F _iohk-ops io
                 '';

in pkgs.lib.overrideDerivation iohk-ops.env
   (old: {
     shellHook = ''
       function io {
        ${ioAlias} "$@"
       }
       ${ioCompletion}
     '';
    })
