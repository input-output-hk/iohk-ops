{ mkDerivation, aeson, attoparsec, base, bytestring, engine-io
, fetchgit, mtl, stdenv, stm, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "socket-io";
  version = "1.3.7";
  src = fetchgit {
    url = "https://github.com/serokell/engine.io.git";
    sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
    rev = "a594e402fd450f11ad60d09ddbd93db500000632";
  };
  postUnpack = "sourceRoot+=/socket-io; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring engine-io mtl stm text
    transformers unordered-containers vector
  ];
  doCheck = false;
  homepage = "http://github.com/ocharles/engine.io";
  license = stdenv.lib.licenses.bsd3;
}
