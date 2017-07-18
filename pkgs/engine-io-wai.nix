{ mkDerivation, attoparsec, base, bytestring, either, engine-io
, fetchgit, http-types, mtl, stdenv, text, transformers
, transformers-compat, unordered-containers, wai, wai-websockets
, websockets
}:
mkDerivation {
  pname = "engine-io-wai";
  version = "1.0.6";
  src = fetchgit {
    url = "https://github.com/serokell/engine.io.git";
    sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
    rev = "a594e402fd450f11ad60d09ddbd93db500000632";
  };
  postUnpack = "sourceRoot+=/engine-io-wai; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring either engine-io http-types mtl text
    transformers transformers-compat unordered-containers wai
    wai-websockets websockets
  ];
  doCheck = false;
  homepage = "http://github.com/ocharles/engine.io";
  license = stdenv.lib.licenses.bsd3;
}
