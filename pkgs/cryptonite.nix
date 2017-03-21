{ mkDerivation, base, bytestring, deepseq, fetchgit, ghc-prim
, integer-gmp, memory, stdenv, tasty, tasty-hunit, tasty-kat
, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptonite";
  version = "0.22";
  src = fetchgit {
    url = "https://github.com/haskell-crypto/cryptonite.git";
    sha256 = "14b64f45fgxq6kdq44ibsw23bzhkfy3arpd0ws468wd7vihdah4v";
    rev = "6440a7ebab7d85612e47099017bee0da6339af05";
  };
  libraryHaskellDepends = [
    base bytestring deepseq ghc-prim integer-gmp memory
  ];
  testHaskellDepends = [
    base bytestring memory tasty tasty-hunit tasty-kat tasty-quickcheck
  ];
  doCheck = false;
  homepage = "https://github.com/haskell-crypto/cryptonite";
  description = "Cryptography Primitives sink";
  license = stdenv.lib.licenses.bsd3;
}
