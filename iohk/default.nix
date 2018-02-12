{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3
, ansi-terminal, base, bytestring, cassava, containers, cryptonite
, directory, dns, errors, git, hourglass, hspec, http-client
, http-client-tls, http-conduit, http-types, lens, lens-aeson
, managed, memory, mtl, network-uri, optional-args
, optparse-applicative, regex-pcre, resourcet, safe, stdenv
, system-filepath, text, turtle, universum, unix
, unordered-containers, utf8-string, vector, yaml
}:
mkDerivation {
  pname = "iohk-ops";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 ansi-terminal base
    bytestring cassava containers cryptonite directory dns errors git
    hourglass http-client http-client-tls http-conduit http-types lens
    lens-aeson managed memory mtl network-uri optional-args
    optparse-applicative regex-pcre resourcet safe system-filepath text
    turtle unix unordered-containers utf8-string vector yaml
  ];
  testHaskellDepends = [
    aeson amazonka amazonka-s3 ansi-terminal base bytestring cassava
    cryptonite directory errors git hspec http-client http-client-tls
    http-conduit http-types lens managed memory network-uri regex-pcre
    resourcet safe system-filepath text turtle universum
    unordered-containers yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
