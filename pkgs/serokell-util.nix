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
  version = "0.1.5.0";
  src = fetchgit {
    url = "https://github.com/serokell/serokell-util.git";
    sha256 = "0vwcnxaxhsdg4ww04lgp6bzjwiprpd55wlypc795rm6s0x02byc3";
    rev = "1309ac5024fb0c62b56c3bd7d16feb0a318a2512";
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
