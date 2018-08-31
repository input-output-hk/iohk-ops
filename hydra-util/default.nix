with import <nixpkgs> {};

let
  hpkgs = haskellPackages.override {
    overrides = self: super: {
    };
  };
  ghc = hpkgs.ghcWithPackages (p: with p; [ diagrams diagrams-cairo aeson postgresql-simple containers turtle diagrams-svg formatting servant-server ]);
in runCommand "hydra-util" { buildInputs = [ ghc hpkgs.ghcid ]; } ''
  cp -vr ${./.}/*.hs .
  mkdir -pv $out/bin
  ghc Main.hs -o $out/bin/hydra-util
''
