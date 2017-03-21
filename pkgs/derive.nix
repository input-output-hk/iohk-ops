{ mkDerivation, base, bytestring, containers, directory, fetchgit
, filepath, haskell-src-exts, pretty, process, stdenv, syb
, template-haskell, transformers, uniplate
}:
mkDerivation {
  pname = "derive";
  version = "2.6.2";
  src = fetchgit {
    url = "https://github.com/ndmitchell/derive.git";
    sha256 = "147pcqgm7p0rs08sdfrka6xdw7h0c8c3dwlfi35scj3yxamcda2s";
    rev = "9b564c23543d92757168581beb832f4dc0db223b";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath haskell-src-exts
    pretty process syb template-haskell transformers uniplate
  ];
  executableHaskellDepends = [ base ];
  doCheck = false;
  homepage = "https://github.com/ndmitchell/derive#readme";
  description = "A program and library to derive instances for data types";
  license = stdenv.lib.licenses.bsd3;
}
