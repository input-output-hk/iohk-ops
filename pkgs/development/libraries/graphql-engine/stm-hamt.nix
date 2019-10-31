{ mkDerivation, async, base, criterion, deferred-folds, fetchgit
, focus, free, hashable, list-t, mwc-random, mwc-random-monad
, primitive, primitive-extras, QuickCheck, quickcheck-instances
, rebase, rerebase, stdenv, tasty, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "stm-hamt";
  version = "1.2.0.4";
  src = fetchgit {
    url = "https://github.com/nikita-volkov/stm-hamt";
    sha256 = "1qf7p5b07nmhaihg1l7ri11dvmgbmcf3ymnc3x16hdhy9g8ldng2";
    rev = "9521b1c8d4b486555fddc47fb0960996caa7dbe1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deferred-folds focus hashable list-t primitive
    primitive-extras transformers
  ];
  testHaskellDepends = [
    deferred-folds focus QuickCheck quickcheck-instances rerebase tasty
    tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    async criterion focus free list-t mwc-random mwc-random-monad
    rebase
  ];
  homepage = "https://github.com/nikita-volkov/stm-hamt";
  description = "STM-specialised Hash Array Mapped Trie";
  license = stdenv.lib.licenses.mit;
}
