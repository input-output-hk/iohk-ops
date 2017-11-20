let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, compiler ? pkgs.haskell.packages.ghc802
, enableDebugging ? false
, enableProfiling ? false
}:

with pkgs.lib;
with pkgs.haskell.lib;

let
  nixops = 
    let
      # nixopsUnstable = /path/to/local/src
      nixopsUnstable = pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixops";
        rev = "000366a468867ccb2ebf031a6decde9436a234fd";
        sha256 = "16ff1zn5s7i0rczbzjxkaahibwf714m4qgg72j1glda82d41clgb";
      };
    in (import "${nixopsUnstable}/release.nix" {
         nixpkgs = localLib.fetchNixPkgs;
        }).build.${system};
  iohk-ops-extra-runtime-deps = [
    pkgs.gitFull pkgs.nix-prefetch-scripts compiler.yaml
    pkgs.wget pkgs.awscli # for scripts/aws.hs
    pkgs.file
    cardano-sl-pkgs.cardano-sl-auxx
    cardano-sl-pkgs.cardano-sl-tools
    nixops
  ];
  # we allow on purpose for cardano-sl to have it's own nixpkgs to avoid rebuilds
  cardano-sl-src = builtins.fromJSON (builtins.readFile ./cardano-sl-src.json);
  cardano-sl-pkgs = import (pkgs.fetchgit cardano-sl-src) {
    gitrev = cardano-sl-src.rev;
    inherit enableDebugging enableProfiling;
  };
in {
  inherit nixops;

  iohk-ops = pkgs.haskell.lib.overrideCabal
             (compiler.callPackage ./iohk/default.nix {})
             (drv: {
                executableToolDepends = [ pkgs.makeWrapper ];
                libraryHaskellDepends = iohk-ops-extra-runtime-deps;
                postInstall = ''
                  wrapProgram $out/bin/iohk-ops \
                  --prefix PATH : "${pkgs.lib.makeBinPath iohk-ops-extra-runtime-deps}"
                '';
             });
} // cardano-sl-pkgs
