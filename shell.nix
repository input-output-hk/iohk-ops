{ ghcVer   ? "ghc802"
, intero   ? false
}: let

lib       = import ./lib.nix;
nixpkgs   = lib.fetchNixPkgs;
pkgs      = import nixpkgs {};
compiler  = pkgs.haskell.packages."${ghcVer}";

ghcOrig   = import ./default.nix { inherit pkgs compiler; };
overcabal = pkgs.haskell.lib.overrideCabal; hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);
ghc       = ghcOrig.override (oldArgs: {
  overrides = with pkgs.haskell.lib; new: old:
  let parent = (oldArgs.overrides or (_: _: {})) new old;
  in with new; parent // {
      intero         = overhub  old.intero "commercialhaskell/intero" "e546ea086d72b5bf8556727e2983930621c3cb3c" "1qv7l5ri3nysrpmnzfssw8wvdvz0f6bmymnz1agr66fplazid4pn" { doCheck = false; };
    };
  });

###
###
###
drvf =
{ mkDerivation, stdenv, src ? ./.
,   aeson, base, cassava, jq, lens-aeson, nix-prefetch-git, safe, turtle, utf8-string, vector, yaml
}:
mkDerivation {
  pname = "iohk-nixops";
  version = "0.0.1";
  src = src;
  isLibrary = false;
  isExecutable = true;
  doHaddock = false;
  executableHaskellDepends = [
    aeson  base  cassava  jq  lens-aeson  nix-prefetch-git  safe  turtle  utf8-string  vector  yaml
  ];
  shellHook =
  ''
    export NIX_PATH=nixpkgs=${nixpkgs}
    echo   NIX_PATH set to $NIX_PATH >&2
  '';
  license      = stdenv.lib.licenses.mit;
};

drv = (pkgs.haskell.lib.addBuildTools
(ghc.callPackage drvf { })
(if intero
 then [ pkgs.cabal-install
        pkgs.stack
        ghc.intero ]
 else []));

in drv.env
