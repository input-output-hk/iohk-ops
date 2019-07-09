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
  sources = localLib.sources;
  nixops =
    let
      # nixopsUnstable = /path/to/local/src
      nixopsUnstable = sources.nixops-iohk;
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

  cardano-sl-pkgs = import (sources.cardano-sl.revOverride cardanoRevOverride) {};
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
    parts = with cardano-sl-pkgs; [ cardano-sl-auxx cardano-sl-tools iohk-ops ];
  in pkgs.runCommand "iohk-ops-integration-test" {} ''
    mkdir -p $out/nix-support
    export PATH=${pkgs.lib.makeBinPath parts}:$PATH
    export CARDANO_SL_CONFIG=${cardano-sl-pkgs.cardano-sl-config}
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
