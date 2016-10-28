{ pkgs } :

with pkgs; rec {
  universum = hspkgs.mkDerivation {
    pname = "universum";
    version = "0.1.8";
    src = fetchgit {
      url = "https://github.com/serokell/universum.git";
      sha256 = "0rg8m41r32a0g0izirj2kmc0c4nry98nsbqdpbr5s7mz13qnh10h";
      rev = "0d50a67184025479df08b207f41710e2e2478e65";
    };
    libraryHaskellDepends = with hspkgs; [
      async base bytestring containers deepseq exceptions ghc-prim mtl
      safe stm text text-format transformers unordered-containers
    ];
    homepage = "https://github.com/serokell/universum";
    description = "A sensible set of defaults for writing custom Preludes";
    license = stdenv.lib.licenses.mit;
  };

  serokell-core = hspkgs.mkDerivation {
    pname = "serokell-core";
    version = "0.1.0.0";
    src = fetchgit {
      url = "https://github.com/serokell/serokell-core.git";
      sha256 = "0b8z4xqqpz2w0423wc2dwp11r7yvcz3n3gsk51m6a846l00kp2mr";
      rev = "7e9cc7f44dccda2736735b24d84b5437899352d3";
    };
    libraryHaskellDepends = with hspkgs; [
      acid-state aeson aeson-extra base base16-bytestring
      base64-bytestring binary binary-orphans bytestring cereal
      cereal-vector clock containers data-msgpack deepseq directory
      either exceptions extra filepath formatting hashable lens mtl
      optparse-applicative parsec QuickCheck quickcheck-instances
      safecopy scientific semigroups template-haskell text text-format
      time-units transformers unordered-containers vector yaml
    ];
    testHaskellDepends = with hspkgs; [
      aeson base binary bytestring cereal hspec msgpack QuickCheck
      quickcheck-instances safecopy scientific text text-format
      unordered-containers vector
    ];
    homepage = "http://gitlab.serokell.io/serokell-team/serokell-core";
    description = "General-purpose functions by Serokell";
    license = stdenv.lib.licenses.mit;
  };

  acid-state = hspkgs.mkDerivation {
    pname = "acid-state";
    version = "0.14.2";
    src = fetchgit {
      url = "https://github.com/serokell/acid-state";
      sha256 = "1d9wig3ziwz2rm4j3yqp3kajim1dxxlp7b16vz1r9cyz6k5hds0n";
      rev = "dea606c78d9ecd22247e3c90dcf12c6b4b0458fc";
    };
    libraryHaskellDepends = with hspkgs; [
      array base bytestring cereal containers directory
      extensible-exceptions filepath mtl network safecopy stm
      template-haskell th-expand-syns unix
    ];
    homepage = "http://acid-state.seize.it/";
    description = "Add ACID guarantees to any serializable Haskell data structure";
    license = stdenv.lib.licenses.publicDomain;
  };

  time-warp = hspkgs.mkDerivation {
    pname = "time-warp";
    version = "0.1.0.0";
    src = fetchgit {
      url = "https://github.com/serokell/time-warp";
      sha256 = "12nj594avq80jlnj88n52pdcrsy5bcbn1v4f3bjb60543na40qdf";
      rev = "86bb875da3eda203baea248aaced08286e535ae1";
    };
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = with hspkgs; [
      ansi-terminal base bytestring containers data-default exceptions
      formatting hslogger lens lifted-base monad-control monad-loops
      MonadRandom msgpack msgpack-rpc mtl pqueue QuickCheck
      quickcheck-instances random safe serokell-core stm template-haskell
      text text-format time time-units transformers transformers-base
    ];
    executableHaskellDepends = with hspkgs; [
      async base data-default exceptions formatting hspec lens
      MonadRandom msgpack msgpack-rpc mtl QuickCheck random serokell-core
      stm text text-format time-units transformers
    ];
    testHaskellDepends = with hspkgs; [
      async base data-default exceptions hspec lens msgpack msgpack-rpc
      mtl QuickCheck random serokell-core stm text text-format time-units
      transformers
    ];
    homepage = "http://gitlab.serokell.io/serokell-team/time-warp";
    description = "distributed systems execution emulation";
    license = stdenv.lib.licenses.gpl3;

  };

  pvss-haskell = hspkgs.mkDerivation { 
    pname = "pvss";
    version = "0.1.0.0";
    src = fetchgit {
      url = "https://github.com/input-output-hk/pvss-haskell";
      sha256 = "105ns64469vkzg120m1kfknrf9nygwqsh0kqfzgqrh67p4846fwi";
      rev = "ec1f50c60feab9c7441f01aa94a006b40972acc6";
    };
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = with hspkgs; [
      base binary bytestring cryptonite cryptonite-openssl deepseq
      integer-gmp memory
    ];
    executableHaskellDepends = with hspkgs; [
      base cryptonite deepseq hourglass memory
    ];
    testHaskellDepends = with hspkgs; [ base cryptonite tasty tasty-quickcheck ];
    homepage = "https://github.com/githubuser/pvss#readme";
    description = "Public Verifiable Secret Sharing";
    license = stdenv.lib.licenses.bsd3;
  };
 
  kademlia = hspkgs.mkDerivation {
    pname = "kademlia";
    version = "1.1.0.0";
    src = fetchgit {
      url = "https://github.com/serokell/kademlia";
      sha256 = "0p0ww5qq5bxfrrhrx3pi5pg62m5nz0am5jqr19gbz78vdc06zk6w";
      rev = "d3d6d62fc248156320ec92c730a38f483500edd5";
    };

    doCheck = false;
    libraryHaskellDepends = with hspkgs; [
      base bytestring containers mtl network stm transformers
      transformers-compat
    ];
    testHaskellDepends = with hspkgs; [
      base bytestring containers HUnit mtl network QuickCheck stm tasty
      tasty-hunit tasty-quickcheck transformers transformers-compat
    ];
    homepage = "https://github.com/froozen/kademlia";
    description = "An implementation of the Kademlia DHT Protocol";
    license = stdenv.lib.licenses.bsd3;
  };

  cardano = hspkgs.mkDerivation {
    pname = "pos";
    version = "0.1.0.0";
    src = fetchgit {
      url = "https://github.com/input-output-hk/pos-haskell-prototype";
      sha256 = "1hd9facr1hvsh7xmdf3g53z3s01xz8va3jpzzmgnysalidp8m7cq";
      rev = "680e30cdc77a54d03283f6b049cd8678c56be97b";
    };
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = with hspkgs; [
      acid-state async base binary binary-orphans bytestring cereal
      containers cryptonite data-default data-msgpack derive ed25519
      exceptions formatting hashable HsOpenSSL kademlia lens lrucache
      memory mtl parsec pvss QuickCheck quickcheck-instances random
      safecopy serokell-core stm template-haskell text text-format time
      time-warp transformers universum unordered-containers vector
    ];
    executableHaskellDepends = with hspkgs; [
      async base binary bytestring data-default directory filepath
      formatting optparse-applicative optparse-simple parsec
      serokell-core time-warp universum
    ];
    testHaskellDepends = with hspkgs; [
      base binary bytestring cryptonite formatting hspec memory
      QuickCheck random serokell-core time-units time-warp universum
      unordered-containers
    ];
    description = "Proof-of-stake";
    license = stdenv.lib.licenses.bsd3;
  };
  
  hspkgs = pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: {
      inherit serokell-core;
      inherit acid-state;
      inherit time-warp;
      inherit universum;
      inherit kademlia; 
      inherit cardano;
      pvss = pvss-haskell;
    };
  };

}

