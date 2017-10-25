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
  pkgs' = import pkgs.path { overlays = [ (pkgsself: pkgssuper: rec {
    python2 = python27;
    python27 = let
      packageOverrides = self: super: {
        botocore = super.botocore.override {
          version = "1.7.21";
          src =  pkgs.fetchFromGitHub {
            owner = "boto";
            repo = "botocore";
            rev = "0a91c843ecef13289bf4d623ff1923505f84fc45"; # v1.7.21, latest
            sha256 = "0fxvpb3lky491znhp5hkc0r7gqj7p3rayqcrq42a7wyb8jwmcpa7";
            # rev = "103604480adc361fc9e875504e739f622ea8ca5b"; # v1.5.95, last in 1.5 branch
            # sha256 = "1akrm4c848fzpi86aca7jb2kd3v14sf7x68h96b95fi76aisrdxc";
          };};
        boto3 = super.boto3.override {
          version = "1.4.7";
          src =  pkgs.fetchFromGitHub {
            owner = "boto";
            repo = "boto3";
            # rev = throw "eval boto3.src.rev";
            rev = "9d5fac2a36a31b9665a8d80c7dfe8df9fac4ceb3"; # v1.4.7, latest
            sha256 = "0ca08xkkx6py08gqgn1aci9pklidwivxbvpwjv7623jr21avakdi";
            # rev = "25756f985b3f398aca71bc54d5ea95491bb11201"; # v1.4.5, same era as v1.5.95 of botocore
            # sha256 = "0q52xnjbpx6iawxhnn7ky2w5xz9bhsmd4v18r9ii748br5f3zwyi";
          };
          doCheck = false; };
      };
    in pkgssuper.python27.override {inherit packageOverrides;};
  } ) ]; };
  nixops = 
    let
      nixopsUnstable = ../nixops;
      # nixopsUnstable = pkgs.fetchFromGitHub {
      #   owner = "NixOS";
      #   repo = "nixops";
      #   rev = "c06c0e79ab8d7a58d80b1c38b7ae4ed1a04322f0";
      #   sha256 = "1fly6ry7ksj7v5rl27jg5mnxdbjwn40kk47gplyvslpvijk65m4q";
      # };
    in (import "${nixopsUnstable}/release.nix" { py2pkgs = pkgs'.python27Packages; }).build.${system};
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
