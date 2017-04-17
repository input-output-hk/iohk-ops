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
    sha256 = "0xslqvfx7hgiwmpp6qsmpi6dflsk2ip1nlgzyc94lr98w2abmzqr";
    rev = "95433016161134a0fb4b6a3e67a75d681d158e0f";
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
