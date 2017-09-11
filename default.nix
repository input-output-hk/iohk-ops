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

let
  iohk-ops-extra-runtime-deps = [
    pkgs.git pkgs.nix-prefetch-scripts compiler.yaml
  ];
  # we allow on purpose for cardano-sl to have it's own nixpkgs to avoid rebuilds
  cardano-sl-src = builtins.fromJSON (builtins.readFile ./cardano-sl-src.json);
  cardano-sl-pkgs = import (pkgs.fetchgit cardano-sl-src) {
                      gitrev = cardano-sl-src.rev;
                      inherit dconfig;
                    };
in {
  nixops = 
    let
      # nixopsUnstable = /path/to/local/src
      nixopsUnstable = pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixops";
        rev = "c06c0e79ab8d7a58d80b1c38b7ae4ed1a04322f0";
        sha256 = "1fly6ry7ksj7v5rl27jg5mnxdbjwn40kk47gplyvslpvijk65m4q";
      };
    in (import "${nixopsUnstable}/release.nix" {}).build.x86_64-linux;
  iohk-ops = pkgs.haskell.lib.overrideCabal
             (compiler.callPackage ./iohk/default.nix {})
             (drv: {
                executableToolDepends = [ pkgs.makeWrapper ];
                postInstall = ''
                  wrapProgram $out/bin/iohk-ops \
                  --prefix PATH : "${pkgs.lib.makeBinPath iohk-ops-extra-runtime-deps}"
                '';
             });
} // cardano-sl-pkgs
