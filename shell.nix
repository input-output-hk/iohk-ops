{ ghcVer   ? "ghc802"
, intero   ? false
}: let

lib           = import ./lib.nix;
nixpkgs       = lib.fetchNixPkgs;
pkgs          = import nixpkgs {};
compiler      = pkgs.haskell.packages."${ghcVer}";

ghcOrig       = import ./default.nix { inherit pkgs compiler; };

githubSrc     =      repo: rev: sha256:       pkgs.fetchgit  { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
overC         =                               pkgs.haskell.lib.overrideCabal;
overCabal     = old:                    args: overC old (oldAttrs: (oldAttrs // args));
overGithub    = old: repo: rev: sha256: args: overC old ({ src = githubSrc repo rev sha256; }     // args);
overHackage   = old: version:   sha256: args: overC old ({ version = version; sha256 = sha256; } // args);

stack2NixSrc  = builtins.fromJSON (builtins.readFile ./stack2nix-src.json);

ghc           = ghcOrig.override (oldArgs: {
  overrides = with pkgs.haskell.lib; new: old:
  let parent = (oldArgs.overrides or (_: _: {})) new old;
  in with new; parent // {
      intero         = overGithub  old.intero "commercialhaskell/intero"
                       "e546ea086d72b5bf8556727e2983930621c3cb3c" "1qv7l5ri3nysrpmnzfssw8wvdvz0f6bmymnz1agr66fplazid4pn" { doCheck = false; };
      cabal2nix      = overGithub compiler.cabal2nix "NixOS/cabal2nix"
                       "b6834fd420e0223d0d57f8f98caeeb6ac088be88" "1ia2iw137sza655b0hf4hghpmjbsg3gz3galpvr5pbbsljp26m6p" {};
      stack2nix      = dontCheck
                       (pkgs.haskellPackages.callCabal2nix "stack2nix"
                        (githubSrc "input-output-hk/stack2nix" stack2NixSrc.rev stack2NixSrc.sha256) {});
    };
  });

###
###
###
drvf =
{ mkDerivation, stdenv, src ? ./.
,   aeson, base, cassava, jq, lens-aeson, nix-prefetch-git, safe, turtle, utf8-string, vector, yaml
,   stack2nix, cabal2nix, cabal-install, intero, interoRequested
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
    stack2nix  cabal2nix  cabal-install
  ] ++
  (if interoRequested
   then [ pkgs.stack intero ]
   else []);
  shellHook =
  ''
    export NIX_PATH=nixpkgs=${nixpkgs}
    echo   NIX_PATH set to $NIX_PATH >&2
  '';
  license      = stdenv.lib.licenses.mit;
};

drv = ghc.callPackage drvf { interoRequested = intero; };

in drv.env
