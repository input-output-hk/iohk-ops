{ mkDerivation, base, binary, bytestring, containers, cryptonite
, data-default, errors, extra, fetchgit, HUnit, memory, MonadRandom
, mtl, network, QuickCheck, random, random-shuffle, stdenv, stm
, tasty, tasty-hunit, tasty-quickcheck, time, transformers
, transformers-compat
}:
mkDerivation {
  pname = "kademlia";
  version = "1.1.0.1";
  src = fetchgit {
    url = "https://github.com/serokell/kademlia.git";
    sha256 = "0sdb6w33vyx86pbp9s5s8c3lwsahcds2vzhpfpn7p9srp82mb82i";
    rev = "bf65ac0cd50d2ccd7ef6507f0d71786c4bd10ae1";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers cryptonite data-default errors
    extra memory MonadRandom mtl network random random-shuffle stm time
    transformers transformers-compat
  ];
  executableHaskellDepends = [
    base binary bytestring containers data-default extra MonadRandom
    mtl network random random-shuffle transformers transformers-compat
  ];
  testHaskellDepends = [
    base binary bytestring containers data-default errors extra HUnit
    MonadRandom mtl network QuickCheck random random-shuffle stm tasty
    tasty-hunit tasty-quickcheck time transformers transformers-compat
  ];
  doCheck = false;
  homepage = "https://github.com/serokell/kademlia";
  description = "An implementation of the Kademlia DHT Protocol";
  license = stdenv.lib.licenses.bsd3;
}
