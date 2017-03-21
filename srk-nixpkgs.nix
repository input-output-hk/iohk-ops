{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
}:

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs;});

let
  prodMode = drv: overrideCabal drv (drv: {
    configureFlags = [ "-f-asserts" "-f-dev-mode"];
  });
in rec {
  hspkgs = compiler.override {
    overrides = self: super: {
      # To generate these go to ./pkgs and run ./generate.sh
      universum = super.callPackage ./pkgs/universum.nix { };
      serokell-util = super.callPackage ./pkgs/serokell-util.nix { };
      acid-state = super.callPackage ./pkgs/acid-state.nix { };
      log-warper = super.callPackage ./pkgs/log-warper.nix { };
      ed25519 = super.callPackage ./pkgs/ed25519.nix { };
      rocksdb = super.callPackage ./pkgs/rocksdb-haskell.nix { rocksdb = pkgs.rocksdb; };
      kademlia = super.callPackage ./pkgs/kademlia.nix { };
      node-sketch = super.callPackage ./pkgs/time-warp-nt.nix { };
      cardano-report-server = super.callPackage ./pkgs/cardano-report-server.nix { };
      cardano-crypto = super.callPackage ./pkgs/cardano-crypto.nix { };
      plutus-prototype = super.callPackage ./pkgs/plutus-prototype.nix { };
      network-transport = super.callPackage ./pkgs/network-transport.nix { };
      network-transport-tcp = super.callPackage ./pkgs/network-transport-tcp.nix { };
      derive = super.callPackage ./pkgs/derive.nix { };
      cryptonite = super.callPackage ./pkgs/cryptonite.nix { };

      # TODO: https://github.com/NixOS/cabal2nix/issues/261
      cardano-sl-core = prodMode (super.callCabal2nix "cardano-sl-core" "${self.cardano-sl.src}/core" {});
      cardano-sl-db = super.callCabal2nix "cardano-sl-db" "${self.cardano-sl.src}/db" {};
      cardano-sl-infra = prodMode (super.callCabal2nix "cardano-sl-infra" "${self.cardano-sl.src}/infra" {});
      cardano-sl-lrc = super.callCabal2nix "cardano-sl-lrc" "${self.cardano-sl.src}/lrc" {};
      cardano-sl-update = super.callCabal2nix "cardano-sl-update" "${self.cardano-sl.src}/update" {};
      cardano-sl-explorer = prodMode (super.callPackage ./pkgs/cardano-sl-explorer.nix { });

      cardano-sl = overrideCabal (super.callPackage ./pkgs/cardano-sl.nix { }) (drv: {
        patchPhase = ''
         export CSL_SYSTEM_TAG=linux64
        '';
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [ "-f-asserts" "-f-dev-mode" "-fwith-explorer"];
      });
      cardano-sl-static = justStaticExecutables self.cardano-sl; 
      cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer; 

      #mkDerivation = args: super.mkDerivation (args // {
      #enableLibraryProfiling = false;
      #});
    };
  };
}
