let
  nixpkgs = import (builtins.fetchTarball https://nixos.org/channels/nixpkgs-18.03-darwin/nixexprs.tar.xz);
  pkgs_mac = nixpkgs {system = "x86_64-darwin";};
  pkgs_native = nixpkgs {};

in rec {
  darwin-tools = let
    ghc = pkgs_mac.haskellPackages.ghcWithPackages (ps: [ ps.turtle ps.universum ps.megaparsec ]);
  in pkgs_mac.stdenv.mkDerivation {
    name = "deploy-nix-darwin";
    buildInputs = [ ghc pkgs_mac.nixStable ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    src = ./.;
    installPhase = ''
      mkdir -p $out/bin
      ghc -o patch-prepare patch-prepare.hs
      ghc -o $out/bin/prepare everything.hs
      ln -s prepare $out/bin/nuke-nix
      ./patch-prepare
    '';
  };
  prepare-mac = pkgs_native.writeScriptBin "prepare-mac" ''
    #!${pkgs_native.stdenv.shell}
    set -e
    ssh -t $1 "chmod -R +w darwin-tools; rm -rf darwin-tools" || true
    scp -r ${darwin-tools}/bin $1:darwin-tools
    ssh -t $1 "sudo darwin-tools/nuke-nix"
    ssh -t $1 "darwin-tools/prepare $2"
  '';
  deploy-darwin = let
    ghc = pkgs_native.haskellPackages.ghcWithPackages (ps: [ ps.turtle ps.universum ps.megaparsec ]);
  in pkgs_native.stdenv.mkDerivation {
    name = "deploy-darwin";
    buildInputs = [ ghc ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    src = ./.;
    installPhase = ''
      mkdir -p $out/bin
      ghc -o $out/bin/deploy deploy-darwin.hs
    '';
  };
  tools = pkgs_native.buildEnv {
    name = "scripts";
    paths = [ prepare-mac deploy-darwin ];
  };
}
