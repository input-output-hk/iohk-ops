let
  localLib = import ./lib.nix;
  nixpkgs  = localLib.fetchNixPkgs;
  pkgs     = import nixpkgs {};
in

{ ... }:
let
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
                echo   NIX_PATH set to $NIX_PATH >&2
            
                alias git-rev='git log -n1 --pretty=oneline'
                echo "Aliases:"                                                                                                        >&2
                echo                                                                                                                   >&2
                echo "  git-rev:              git log -n1 --pretty=oneline"                                                            >&2
                echo                                                                                                                   >&2
              '';
             });
in
  drv''
