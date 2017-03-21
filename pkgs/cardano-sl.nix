{ mkDerivation, acid-state, aeson, ansi-terminal, array, async
, attoparsec, base, base58-bytestring, base64-bytestring, binary
, binary-conduit, binary-orphans, bytestring, cardano-crypto
, cardano-report-server, cardano-sl-core, cardano-sl-db
, cardano-sl-infra, cardano-sl-lrc, cardano-sl-update, cereal
, concurrent-extra, conduit, containers, cpphs, cryptonite
, cryptonite-openssl, data-default, deepseq, derive
, deriving-compat, digest, directory, dlist, ed25519, exceptions
, fetchgit, file-embed, filelock, filepath, focus, foldl
, formatting, gitrev, hashable, hspec, http-client, http-client-tls
, http-conduit, http-types, IfElse, kademlia, lens, lifted-async
, list-t, log-warper, lrucache, memory, mmorph, monad-control
, monad-loops, mono-traversable, mtl, neat-interpolation
, network-info, network-transport, network-transport-tcp
, node-sketch, optparse-applicative, optparse-simple, parsec
, plutus-prototype, process, purescript-bridge, pvss, QuickCheck
, quickcheck-instances, random, random-shuffle, reflection
, regex-tdfa, regex-tdfa-text, resourcet, rocksdb, safecopy
, serokell-util, servant, servant-docs, servant-server, stdenv, stm
, stm-containers, system-filepath, tagged, tar, template-haskell
, temporary, text, text-format, th-lift-instances, time, time-units
, transformers, transformers-base, turtle, universum, unix
, unordered-containers, vector, wai, wai-extra, wai-websockets
, warp, websockets, wreq, yaml
}:
mkDerivation {
  pname = "cardano-sl";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl.git";
    sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
    rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson ansi-terminal async base base58-bytestring
    base64-bytestring binary binary-conduit binary-orphans bytestring
    cardano-crypto cardano-report-server cardano-sl-core cardano-sl-db
    cardano-sl-infra cardano-sl-lrc cardano-sl-update cereal
    concurrent-extra conduit containers cryptonite cryptonite-openssl
    data-default deepseq derive deriving-compat digest directory dlist
    ed25519 exceptions file-embed filelock filepath focus formatting
    gitrev hashable http-client http-client-tls http-conduit http-types
    IfElse kademlia lens lifted-async list-t log-warper lrucache memory
    mmorph monad-control monad-loops mono-traversable mtl
    neat-interpolation network-info network-transport
    network-transport-tcp node-sketch optparse-applicative
    optparse-simple parsec plutus-prototype pvss QuickCheck
    quickcheck-instances random reflection resourcet rocksdb safecopy
    serokell-util servant servant-docs servant-server stm
    stm-containers tagged template-haskell temporary text text-format
    th-lift-instances time time-units transformers transformers-base
    turtle universum unordered-containers vector wai wai-extra
    wai-websockets warp websockets wreq yaml
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    aeson array async attoparsec base base58-bytestring binary
    bytestring cardano-report-server cardano-sl-core cardano-sl-infra
    cardano-sl-lrc cardano-sl-update containers cryptonite data-default
    directory ed25519 filepath foldl formatting kademlia lens
    lifted-async log-warper mtl network-transport node-sketch
    optparse-applicative optparse-simple parsec process
    purescript-bridge QuickCheck random random-shuffle serokell-util
    stm system-filepath tar text time time-units transformers turtle
    universum unix unordered-containers vector wreq
  ];
  executableToolDepends = [ cpphs ];
  testHaskellDepends = [
    base binary bytestring cardano-sl-core cardano-sl-infra cereal
    containers cryptonite data-default derive formatting hspec kademlia
    lens log-warper memory mtl node-sketch pvss QuickCheck
    quickcheck-instances random reflection regex-tdfa regex-tdfa-text
    safecopy serokell-util time-units universum unordered-containers
    vector
  ];
  testToolDepends = [ cpphs ];
  doCheck = false;
  description = "Cardano SL main implementation";
  license = stdenv.lib.licenses.mit;
}
