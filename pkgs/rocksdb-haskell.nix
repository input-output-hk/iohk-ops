{ mkDerivation, base, binary, bytestring, data-default, fetchgit
, filepath, hspec, hspec-expectations, process, QuickCheck
, resourcet, rocksdb, stdenv, temporary, transformers
}:
mkDerivation {
  pname = "rocksdb";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/serokell/rocksdb-haskell.git";
    sha256 = "05dwx4wgiqin3ryyjf9z49vxnqs7qzyg296gbbhx3ikg3vxqb3bg";
    rev = "4dfd8d61263d78a91168e86e8005eb9b7069389e";
  };
  libraryHaskellDepends = [
    base binary bytestring data-default filepath resourcet transformers
  ];
  librarySystemDepends = [ rocksdb ];
  testHaskellDepends = [
    base bytestring data-default hspec hspec-expectations process
    QuickCheck resourcet temporary transformers
  ];
  homepage = "http://github.com/serokell/rocksdb-haskell";
  description = "Haskell bindings to RocksDB";
  license = stdenv.lib.licenses.bsd3;
}
