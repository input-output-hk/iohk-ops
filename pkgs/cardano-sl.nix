{ mkDerivation, acid-state, aeson, ansi-terminal, array, async
, attoparsec, base, base58-bytestring, base64-bytestring, binary
, binary-conduit, binary-orphans, bytestring, cardano-crypto
, cardano-report-server, cardano-sl-core, cardano-sl-db
, cardano-sl-infra, cardano-sl-lrc, cardano-sl-update, cereal
, concurrent-extra, conduit, containers, cpphs, cryptonite
, cryptonite-openssl, data-default, deepseq, derive
, deriving-compat, digest, directory, dlist, ed25519, exceptions
, fetchgit, file-embed, filelock, filepath, focus, foldl
, formatting, gitrev, Glob, hashable, hspec, http-client
, http-client-tls, http-conduit, http-types, IfElse, kademlia, lens
, lifted-async, list-t, log-warper, lrucache, memory, mmorph
, monad-control, monad-loops, mono-traversable, mtl
, neat-interpolation, network-info, network-transport
, network-transport-tcp, node-sketch, optparse-applicative
, optparse-simple, parsec, plutus-prototype, process
, purescript-bridge, pvss, QuickCheck, quickcheck-instances, random
, random-shuffle, reflection, regex-tdfa, regex-tdfa-text
, resourcet, rocksdb, safecopy, serokell-util, servant
, servant-docs, servant-multipart, servant-server, servant-swagger
, servant-swagger-ui, stdenv, stm, stm-containers, swagger2
, system-filepath, tagged, tar, template-haskell, temporary, text
, text-format, th-lift-instances, time, time-units, transformers
, transformers-base, turtle, universum, unix, unordered-containers
, vector, wai, wai-extra, wai-websockets, warp, websockets, wreq
, yaml
}:
mkDerivation {
  pname = "cardano-sl";
  version = "0.4.1";
  src = fetchgit {
    url = "https://github.com/input-output-hk/cardano-sl.git";
    sha256 = "14ji20s09gcikjr254w8ah9y6k23397z2nxh2mf2nfqihfyh3zza";
    rev = "be7cb65f71e7bd5b34778652009469c4513ecb79";
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
    serokell-util servant servant-docs servant-multipart servant-server
    stm stm-containers tagged template-haskell temporary text
    text-format th-lift-instances time time-units transformers
    transformers-base turtle universum unix unordered-containers vector
    wai wai-extra wai-websockets warp websockets wreq yaml
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    aeson array async attoparsec base base58-bytestring binary
    bytestring cardano-report-server cardano-sl-core cardano-sl-infra
    cardano-sl-lrc cardano-sl-update containers cryptonite data-default
    directory ed25519 filepath foldl formatting Glob kademlia lens
    lifted-async log-warper mtl network-transport node-sketch
    optparse-applicative optparse-simple parsec process
    purescript-bridge QuickCheck random random-shuffle serokell-util
    servant-multipart servant-server servant-swagger servant-swagger-ui
    stm swagger2 system-filepath tar text time time-units transformers
    turtle universum unix unordered-containers vector wreq
  ];
  executableToolDepends = [ cpphs ];
  testHaskellDepends = [
    base binary bytestring cardano-sl-core cardano-sl-infra
    cardano-sl-update cereal containers cryptonite data-default derive
    formatting hspec kademlia lens log-warper memory mtl node-sketch
    pvss QuickCheck quickcheck-instances random reflection regex-tdfa
    regex-tdfa-text safecopy serokell-util time-units universum
    unordered-containers vector
  ];
  testToolDepends = [ cpphs ];
  doCheck = false;
  description = "Cardano SL main implementation";
  license = stdenv.lib.licenses.mit;
}
