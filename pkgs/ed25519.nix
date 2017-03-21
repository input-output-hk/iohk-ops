{ mkDerivation, base, bytestring, directory, doctest, fetchgit
, filemanip, filepath, ghc-prim, hlint, QuickCheck, stdenv
}:
mkDerivation {
  pname = "ed25519";
  version = "0.0.5.0";
  src = fetchgit {
    url = "https://github.com/thoughtpolice/hs-ed25519.git";
    sha256 = "0fah4vkmqdkjsdh3s3x27yfaif2fbdg6049xvp54b5mh50yvxkfq";
    rev = "da4247b5b3420120e20451e6a252e2a2ca15b43c";
  };
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  testHaskellDepends = [
    base bytestring directory doctest filemanip filepath hlint
    QuickCheck
  ];
  homepage = "https://thoughtpolice.github.com/hs-ed25519";
  description = "Ed25519 cryptographic signatures";
  license = stdenv.lib.licenses.mit;
}
