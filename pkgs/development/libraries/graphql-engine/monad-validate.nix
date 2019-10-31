{ mkDerivation, aeson, aeson-qq, base, exceptions, fetchgit, hpack
, hspec, monad-control, mtl, scientific, stdenv, text, transformers
, transformers-base, unordered-containers, vector
}:
mkDerivation {
  pname = "monad-validate";
  version = "1.2.0.0";
  src = fetchgit {
    url = "https://github.com/hasura/monad-validate";
    sha256 = "13z570av3h2mw44wmvqfc5g2m6xwswhljbhbhs361hihmg0yqaal";
    rev = "c2a470015b263f90fa30ed00559be394f4c544f8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base exceptions monad-control mtl transformers transformers-base
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-qq base exceptions hspec monad-control mtl scientific
    text transformers transformers-base unordered-containers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hasura/monad-validate#readme";
  description = "A monad transformer for data validation";
  license = stdenv.lib.licenses.isc;
}
