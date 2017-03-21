{ mkDerivation, base, bytestring, cryptonite, cryptonite-openssl
, deepseq, fetchgit, hashable, memory, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "cardano-crypto";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-crypto";
    sha256 = "02gjaj7889y30g2qfdh96ywrsdpmgfgyqyajw49zaklwjvkh87sv";
    rev = "838b064d8a59286142aa2fe14434fe7601896ddb";
  };
  libraryHaskellDepends = [
    base bytestring cryptonite cryptonite-openssl deepseq hashable
    memory
  ];
  testHaskellDepends = [
    base bytestring cryptonite memory tasty tasty-quickcheck
  ];
  doCheck = false;
  homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
  description = "Cryptography primitives for cardano";
  license = stdenv.lib.licenses.mit;
}
