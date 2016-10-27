{ pkgs } :

with pkgs; rec {
  universum = haskellPackagesExtended.mkDerivation {
    pname = "universum";
    version = "0.1.8";
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/universum.git";
      rev = "500a168f4cffb7b11d8fdcae30e837e937bf83e4";
      sha256 = "1952pmykwsqnbh3hhcx11dypvgi892fbl0alfg2q678v7y61zk44";
    };

    libraryHaskellDepends = with haskellPackagesExtended; [
      base bytestring mtl containers text async safe stm unordered-containers
    ];
    description = "Serokell fork of protolude";
    license = stdenv.lib.licenses.gpl3;
  };

  serokell-core = haskellPackagesExtended.mkDerivation {
    pname = "serokell-core";
    version = "0.1.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/serokell-core.git";
      rev = "4109abf141d427a4b741e42947dd3ab14a3ce5b4";
      sha256 = "0h373gsc19k16k5si83wfxm1d8cbqs548xqp8mhvy77a7qzl42qh";
    };

    isLibrary = true;
    isExecutable = false;

    libraryHaskellDepends = with haskellPackagesExtended; [
       aeson-extra acid-state base64-bytestring 
       either lens optparse-applicative time-units
       either aeson clock formatting
       base16-bytestring binary-orphans cereal-vector msgpack 
       yaml QuickCheck quickcheck-instances
    ];
    license = pkgs.stdenv.lib.licenses.gpl3;
  };

  acid-state = haskellPackagesExtended.mkDerivation {
    pname = "acid-state";
    version = "0.14.1";
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/acid-state.git";
      rev = "dea606c78d9ecd22247e3c90dcf12c6b4b0458fc";
      sha256 = "1d9wig3ziwz2rm4j3yqp3kajim1dxxlp7b16vz1r9cyz6k5hds0n";
    };

    isLibrary = true;
    doCheck = false;

    libraryHaskellDepends = with haskellPackagesExtended; [
       array base bytestring cereal containers directory extensible-exceptions 
       th-expand-syns filepath mtl network safecopy stm template-haskell unix
    ];
    license = pkgs.stdenv.lib.licenses.publicDomain;
  };

  time-warp = haskellPackagesExtended.mkDerivation {
    pname = "time-warp";
    version = "0.1.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/time-warp";
      rev = "390652b1cfcbdb05854a2fca6f930a0d3d4bb711";
      sha256 = "17zvyn7748p63rf14q9pvab8c0qrd3py2l6wbfmgpiisbgy9liji";
    };
    libraryHaskellDepends = with haskellPackagesExtended; [
      ansi-terminal base base64-bytestring binary binary-orphans
      bytestring cereal conduit-extra containers data-default directory
      either exceptions extra file-embed filepath formatting hashable
      hslogger lens lifted-base monad-control monad-loops MonadRandom
      msgpack msgpack-rpc mtl pkgconfig pqueue QuickCheck quickcheck-instances
      random safe safecopy serokell-core stm template-haskell text
      text-format time time-units transformers transformers-base tuple
      unordered-containers vector warp websockets yaml zlib zlib.out
    ];
    testHaskellDepends = with haskellPackagesExtended; [
      aeson async base binary bytestring containers data-default either
      exceptions extra formatting hashable hspec lens MonadRandom msgpack
      msgpack-rpc mtl pkgconfig QuickCheck random safe safecopy serokell-core stm
      text text-format time-units transformers unordered-containers
      vector zlib zlib.out 
    ];
    libraryPkgconfigDepends = with pkgs; [zlib zlib.out];
    isLibrary = true;
    doCheck = false;
    homepage = "http://gitlab.serokell.io/serokell-team/time-warp";
    description = "TODO";
    license = stdenv.lib.licenses.gpl3;
  };
  
  haskellPackagesExtended = pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: {
      ihnerit serokell-core acid-state time-warp universum;
    };
  };

}

