{ mkDerivation, aeson, base, base16-bytestring, bytestring
, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra
, cpphs, exceptions, fetchgit, formatting, lens, log-warper
, monad-loops, mtl, node-sketch, optparse-simple, purescript-bridge
, serokell-util, servant, servant-server, stdenv, stm, text-format
, time, transformers, universum, unordered-containers, wai
}:
mkDerivation {
  pname = "cardano-sl-explorer";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl-explorer.git";
    sha256 = "1faj966kws8mh57c7sz6q3ljsmn3yi411913pxrh1kwkg1lq828n";
    rev = "349cf68b7453430f649880eb4dabd2ed0b55694f";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring cardano-sl cardano-sl-core
    cardano-sl-db cardano-sl-infra exceptions formatting lens
    log-warper monad-loops mtl node-sketch servant servant-server stm
    text-format time transformers universum unordered-containers wai
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    base cardano-sl cardano-sl-core cardano-sl-infra lens log-warper
    node-sketch optparse-simple purescript-bridge serokell-util
    universum
  ];
  executableToolDepends = [ cpphs ];
  doCheck = false;
  description = "Cardano SL main implementation";
  license = stdenv.lib.licenses.mit;
}
