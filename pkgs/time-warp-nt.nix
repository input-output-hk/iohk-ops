{ mkDerivation, async, attoparsec, base, binary, bytestring
, containers, criterion, data-default, deepseq, ekg, ekg-core
, exceptions, fetchgit, formatting, hashable, hspec, kademlia, lens
, log-warper, mmorph, monad-control, mtl, mwc-random, network
, network-transport, network-transport-inmemory
, network-transport-tcp, QuickCheck, quickcheck-instances, random
, semigroups, serokell-util, statistics, stdenv, stm, text
, text-format, time, time-units, transformers, universum
, unordered-containers, vector
}:
mkDerivation {
  pname = "node-sketch";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/serokell/time-warp-nt.git";
    sha256 = "0xl4ydbnl187ghpyvgbyi7lf9hw7xhy638ygpzr4d90nav8xmf1q";
    rev = "2943e7814b9f78b9ef68c9bdc96820ed32a0fdf3";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary bytestring containers data-default
    deepseq ekg ekg-core exceptions formatting hashable kademlia lens
    log-warper mmorph monad-control mtl mwc-random network
    network-transport network-transport-tcp random semigroups
    serokell-util statistics stm text text-format time time-units
    transformers universum unordered-containers vector
  ];
  executableHaskellDepends = [
    async base binary bytestring containers criterion mwc-random
    network-transport network-transport-tcp random statistics stm time
    time-units vector
  ];
  testHaskellDepends = [
    base binary bytestring containers hspec lens mtl network-transport
    network-transport-inmemory network-transport-tcp QuickCheck
    quickcheck-instances random serokell-util stm time-units
  ];
  doCheck = false;
  license = stdenv.lib.licenses.mit;
}
