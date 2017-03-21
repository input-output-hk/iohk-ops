{ mkDerivation, base, bifunctors, binary, bytestring
, cardano-crypto, cryptonite, fetchgit, filepath, lens, memory, mtl
, parsec, stdenv, transformers
}:
mkDerivation {
  pname = "plutus-prototype";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/plutus-prototype.git";
    sha256 = "0h2zq1kcss3f43yqhbz8bjpyxfqlf1wkkdwr91vdkcbjmbgkm8hb";
    rev = "e2e2711e6978002279b4d7c49cab1aff47a2fd43";
  };
  libraryHaskellDepends = [
    base bifunctors binary bytestring cardano-crypto cryptonite
    filepath lens memory mtl parsec transformers
  ];
  homepage = "iohk.io";
  description = "Prototype of the Plutus language";
  license = stdenv.lib.licenses.mit;
}
