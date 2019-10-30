let
  commonLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? (import (commonLib.nixpkgs) { inherit system crossSystem config; })
, compiler ? pkgs.haskellPackages
, cardanoRevOverride ? null
, cardanoNodeRevOverride ? null
, ...
}@args:

with pkgs.lib;
with pkgs.haskell.lib;

let
  # nixopsUnstable = /path/to/local/src
  sources = commonLib.sources;
  nixopsPacketSrc = sources.nixops-packet;
  nixopsSrc = sources.nixops-core; # nixops is an input to iohk-ops on hydra so different name
  haskellOverlay = hself: hsuper: {
    amazonka-core = pkgs.haskell.lib.appendPatch hsuper.amazonka-core ./iohk/amazonka-content-length.patch;
  };
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: {})) haskellOverlay;
  });
  log-classifier-web = (import log-classifier-src {}).haskellPackages.log-classifier-web;
  nixops =
    let
    in (import "${nixopsSrc}/release.nix" {
         inherit (commonLib) nixpkgs;
         p = (p: let
           nixopsPacket = p.callPackage "${nixopsPacketSrc}/release.nix" {};
         in [ p.aws nixopsPacket ]);
        }).build.${system};
  log-classifier-src = sources.log-classifier;
  iohk-ops-extra-runtime-deps = with pkgs; [
    gitFull nix-prefetch-scripts compiler.yaml
    wget
    file
    nixops
    coreutils
    gnupg
  ];

  cardano-sl-src = sources.cardano-sl.revOverride cardanoRevOverride;
  cardano-sl-pkgs = import cardano-sl-src {
    gitrev = cardano-sl-src.rev;
  };
  mantis-pkgs     = commonLib.fetchProjectPackages "mantis"     <mantis>     ./goguen/pins   mantisRevOverride  args;
  cardano-node-pkgs = import (sources.cardano-node.revOverride cardanoNodeRevOverride) {};

  iohk-ops = pkgs.haskell.lib.overrideCabal
             (haskellPackages.callPackage ./iohk {})
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
  IFDPins = let
    f = k: s: "ln -sv ${s.outPath} $out/${k}";
    mapSources = concatStringsSep "\n" (mapAttrsFlatten f sources);
  in pkgs.runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${mapSources}
  '';

  cachecacheSrc = sources.cachecache;
  cachecache = pkgs.callPackage cachecacheSrc {};
in {
  inherit nixops iohk-ops iohk-ops-integration-test log-classifier-web cachecache IFDPins cardano-node-pkgs;
  terraform = pkgs.callPackage ./terraform/terraform.nix {};
  mfa = pkgs.callPackage ./terraform/mfa.nix {};

  checks = let
    src = commonLib.cleanSourceTree ./.;
  in {
    shellcheck = pkgs.callPackage ./scripts/shellcheck.nix { inherit src; };
  };
} // cardano-sl-pkgs
