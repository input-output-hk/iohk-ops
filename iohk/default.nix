{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3
, ansi-terminal, base, bytestring, cassava, containers, cryptonite
, directory, dns, errors, exceptions, foldl, hourglass, hspec
, http-client, http-client-tls, http-conduit, http-types, lens
, lens-aeson, managed, memory, mtl, network-uri, optional-args
, optparse-applicative, process, resourcet, safe, stdenv
, system-filepath, text, time, turtle, unordered-containers
, utf8-string, vector, yaml
}:
mkDerivation {
  pname = "iohk-ops";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 ansi-terminal base
    bytestring cassava containers cryptonite directory dns errors
    exceptions foldl hourglass http-client http-client-tls http-conduit
    http-types lens lens-aeson memory network-uri optional-args
    optparse-applicative resourcet safe system-filepath text turtle
    unordered-containers utf8-string vector yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring containers foldl hourglass hspec managed
    optional-args optparse-applicative process system-filepath text
    time turtle unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base bytestring foldl hspec mtl optparse-applicative process
    system-filepath text time turtle unordered-containers yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
