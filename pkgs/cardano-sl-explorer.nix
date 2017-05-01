{ mkDerivation, aeson, base, base16-bytestring, binary, bytestring
, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra
, containers, cpphs, either, engine-io, engine-io-snap
, engine-io-wai, exceptions, fetchgit, formatting, lens
, lifted-base, log-warper, monad-control, monad-loops, mtl
, node-sketch, optparse-simple, purescript-bridge, serokell-util
, servant, servant-docs, servant-server, snap-core, snap-cors
, snap-server, socket-io, stdenv, stm, text, text-format, time
, time-units, transformers, universum, unordered-containers, wai
, warp
}:
mkDerivation {
  pname = "cardano-sl-explorer";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl-explorer.git";
    sha256 = "0pdbdx4j8qb3kb5pzihk11b81s5yaj4nsds7dmi2f0bszz3jw75a";
    rev = "e3bb99dc5427d4e386859f4e22da72d3686d5d50";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring binary bytestring cardano-sl
    cardano-sl-core cardano-sl-db cardano-sl-infra containers either
    engine-io engine-io-snap engine-io-wai exceptions formatting lens
    lifted-base log-warper monad-control monad-loops mtl node-sketch
    serokell-util servant servant-docs servant-server snap-core
    snap-cors snap-server socket-io stm text text-format time
    time-units transformers universum unordered-containers wai warp
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    base cardano-sl cardano-sl-core cardano-sl-infra formatting lens
    log-warper node-sketch optparse-simple purescript-bridge
    serokell-util universum
  ];
  executableToolDepends = [ cpphs ];
  doCheck = false;
  description = "Cardano explorer";
  license = stdenv.lib.licenses.mit;
}
