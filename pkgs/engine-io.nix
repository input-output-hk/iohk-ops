{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, bytestring, either, fetchgit, free, monad-loops, mwc-random
, stdenv, stm, stm-delay, text, transformers, unordered-containers
, vector, websockets
}:
mkDerivation {
  pname = "engine-io";
  version = "1.2.15";
  src = fetchgit {
    url = "https://github.com/serokell/engine.io.git";
    sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
    rev = "a594e402fd450f11ad60d09ddbd93db500000632";
  };
  postUnpack = "sourceRoot+=/engine-io; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson async attoparsec base base64-bytestring bytestring either
    free monad-loops mwc-random stm stm-delay text transformers
    unordered-containers vector websockets
  ];
  doCheck = false;
  homepage = "http://github.com/ocharles/engine.io";
  description = "A Haskell implementation of Engine.IO";
  license = stdenv.lib.licenses.bsd3;
}
