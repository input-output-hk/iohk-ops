{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, directory, exceptions, fetchgit, filelock
, filepath, formatting, hspec, http-types, HUnit, lens, lifted-base
, log-warper, monad-control, mtl, network, optparse-applicative
, optparse-simple, parsec, QuickCheck, quickcheck-text, random
, scotty, stdenv, text, time, transformers, universum, vector, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "cardano-report-server";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-report-server.git";
    sha256 = "11wq4wjp45mn45x3109xi64p824mlimjkd4dk6r7gnmd0l9v77mh";
    rev = "424e4ecacdf038a01542025dd1296bd272ce770d";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive directory
    exceptions filelock filepath formatting http-types lens lifted-base
    log-warper monad-control mtl network optparse-applicative
    optparse-simple parsec random scotty text time transformers
    universum vector wai wai-extra warp
  ];
  executableHaskellDepends = [
    base directory filepath http-types log-warper monad-control mtl
    optparse-applicative optparse-simple parsec random scotty universum
    wai-extra warp
  ];
  testHaskellDepends = [
    aeson base hspec HUnit lens QuickCheck quickcheck-text text time
    transformers universum
  ];
  homepage = "https://github.com/input-output-hk/cardano-report-server";
  description = "Reporting server for CSL";
  license = stdenv.lib.licenses.bsd3;
}
