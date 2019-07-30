let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? (import (localLib.nixpkgs) { inherit system crossSystem config; })
, compiler ? pkgs.haskellPackages
, cardanoRevOverride ? null
, cardanoNodeRevOverride ? null
, ...
}@args:

with pkgs.lib;
with pkgs.haskell.lib;

let
  nixops =
    let
      # nixopsUnstable = /path/to/local/src
      nixopsUnstable = pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "nixops";
        rev = "ab86e522373f133e2412bd28a864989eb48f58ec";
        sha256 = "0xfwyh21x6r2x7rjgf951gkkld3h10x05qr79im3gvhsgnq3nzmv";
      };
    in (import "${nixopsUnstable}/release.nix" {
         inherit (localLib) nixpkgs;
        }).build.${system};
  iohk-ops-extra-runtime-deps = with pkgs; [
    gitFull nix-prefetch-scripts compiler.yaml
    wget
    file
    nixops
    coreutils
    gnupg
  ];

  cardano-sl-pkgs = localLib.fetchProjectPackages "cardano-sl" <cardano-sl> ./.             cardanoRevOverride args;
  mantis-pkgs     = localLib.fetchProjectPackages "mantis"     <mantis>     ./goguen/pins   mantisRevOverride  args;
  cardano-node-pkgs = localLib.fetchProjectPackages "cardano-node" <cardano-node> ./.       cardanoNodeRevOverride args;

  github-webhook-util = pkgs.callPackage ./github-webhook-util { };

  iohk-ops = pkgs.haskell.lib.overrideCabal
             (compiler.callPackage ./iohk/default.nix {})
             (drv: {
                executableToolDepends = [ pkgs.makeWrapper ];
                libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ iohk-ops-extra-runtime-deps;
                postInstall = ''
                  cp -vs $out/bin/iohk-ops $out/bin/io
                  wrapProgram $out/bin/iohk-ops \
                  --prefix PATH : "${pkgs.lib.makeBinPath iohk-ops-extra-runtime-deps}"
                '';
             });

  iohk-ops-integration-test = let
    parts = with cardano-sl-pkgs.nix-tools.exes; [ cardano-sl-auxx cardano-sl-tools iohk-ops ];
  in pkgs.runCommand "iohk-ops-integration-test" {} ''
    mkdir -p $out/nix-support
    export PATH=${pkgs.lib.makeBinPath parts}:$PATH
    export CARDANO_SL_CONFIG=${cardano-sl-pkgs.cardanoConfig}
    mkdir test
    cp ${iohk-ops.src}/test/Spec.hs test  # a file to hash
    iohk-ops-integration-test | tee $out/test.log
    if [ $? -ne 0 ]; then
      touch $out/nix-support/failed
    fi
    exit 0
  '';

in {
  inherit nixops iohk-ops iohk-ops-integration-test github-webhook-util cardano-node-pkgs;
  terraform = pkgs.callPackage ./terraform/terraform.nix {};
  mfa = pkgs.callPackage ./terraform/mfa.nix {};

  checks = let
    src = localLib.cleanSourceTree ./.;
  in {
    shellcheck = pkgs.callPackage ./scripts/shellcheck.nix { inherit src; };
  };
} // cardano-sl-pkgs
