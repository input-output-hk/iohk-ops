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
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl-explorer.git";
    sha256 = "0l8gwgbqkzfxmcrk6qzj23cz00g61r6bmh0gr3amdlgwcwn2r8v3";
    rev = "24cd9bf6b3b02efd9f81a4791a6f42de8ab23e8a";
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
