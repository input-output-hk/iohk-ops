{ mkDerivation, aeson, base, base16-bytestring, bytestring
, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra
, cpphs, either, exceptions, fetchgit, formatting, lens, log-warper
, monad-loops, mtl, node-sketch, optparse-simple, purescript-bridge
, serokell-util, servant, servant-docs, servant-server, stdenv, stm
, text-format, time, transformers, universum, unordered-containers
, wai
}:
mkDerivation {
  pname = "cardano-sl-explorer";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl-explorer.git";
    sha256 = "0wr48br9p8c5f30z2ls37k417n2ww6hqi3sqarijbanvc2d7qna3";
    rev = "3ff32b51fef5a89722598a65acc47218edd44683";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring cardano-sl cardano-sl-core
    cardano-sl-db cardano-sl-infra either exceptions formatting lens
    log-warper monad-loops mtl node-sketch servant servant-docs
    servant-server stm text-format time transformers universum
    unordered-containers wai
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
