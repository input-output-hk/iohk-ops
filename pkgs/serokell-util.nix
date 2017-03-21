{ mkDerivation, acid-state, aeson, aeson-extra, ansi-terminal, base
, base16-bytestring, base64-bytestring, binary, binary-orphans
, bytestring, cereal, cereal-vector, clock, containers
, data-msgpack, deepseq, directory, either, exceptions, extra
, fetchgit, filepath, formatting, hashable, hspec, lens, log-warper
, monad-control, mtl, optparse-applicative, parsec, QuickCheck
, quickcheck-instances, safecopy, scientific, semigroups, stdenv
, stm, template-haskell, text, text-format, time-units
, transformers, universum, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "serokell-util";
  version = "0.1.4.0";
  src = fetchgit {
    url = "https://github.com/serokell/serokell-util.git";
    sha256 = "1jlrx2cnlxkglgqkkggsfpjpppvnqn4rcj73jj534dfsd1hh9vv8";
    rev = "af79ca0c42b8fd23c499096f91f649edfb0862f1";
  };
  libraryHaskellDepends = [
    acid-state aeson aeson-extra ansi-terminal base base16-bytestring
    base64-bytestring binary binary-orphans bytestring cereal
    cereal-vector clock containers data-msgpack deepseq directory
    either exceptions extra filepath formatting hashable lens
    log-warper monad-control mtl optparse-applicative parsec QuickCheck
    quickcheck-instances safecopy scientific semigroups stm
    template-haskell text text-format time-units transformers universum
    unordered-containers vector yaml
  ];
  testHaskellDepends = [
    aeson base binary bytestring cereal data-msgpack hspec QuickCheck
    quickcheck-instances safecopy scientific text text-format
    unordered-containers vector
  ];
  homepage = "https://github.com/serokell/serokell-util";
  description = "General-purpose functions by Serokell";
  license = stdenv.lib.licenses.mit;
}
