let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, compiler ? pkgs.haskell.packages.ghc802
, dconfig ? "testnet_staging_full"
}:

with pkgs.lib;
with pkgs.haskell.lib;
# with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; lib = pkgs.lib;});

let
  iohk-ops-extra-runtime-deps = [
    pkgs.git pkgs.nix-prefetch-scripts compiler.yaml
  ];
  # we allow on purpose for cardano-sl to have it's own nixpkgs to avoid rebuilds
  cardano-sl-pkgs = import (pkgs.fetchgit (builtins.fromJSON (builtins.readFile ./cardano-sl-src.json))) {
                      inherit dconfig;
                    };
in {
  iohk-ops = pkgs.haskell.lib.overrideCabal
             (compiler.callPackage ./iohk/default.nix {})
             (drv: {
                executableToolDepends = [ pkgs.makeWrapper ];
                postInstall = ''
                  wrapProgram $out/bin/iohk-ops \
                  --prefix PATH : "${pkgs.lib.makeBinPath iohk-ops-extra-runtime-deps}"
                '';
             });
} // cardano-sl-pkgs // {
  cardano-sl-tools-static      = justStaticExecutables cardano-sl-pkgs.cardano-sl-tools;
  cardano-report-server-static = justStaticExecutables cardano-sl-pkgs.cardano-report-server;
}
