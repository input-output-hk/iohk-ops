{ mkDerivation, aeson, ansi-terminal, async, base, bytestring
, containers, data-default, directory, dlist, errors, exceptions
, extra, fetchgit, filepath, formatting, hashable, hspec, HUnit
, lens, monad-control, monad-loops, mtl, network, QuickCheck
, safecopy, stdenv, text, text-format, time, transformers
, transformers-base, universum, unix, unordered-containers, yaml
}:
mkDerivation {
  pname = "log-warper";
  version = "1.0.3";
  src = fetchgit {
    url = "https://github.com/serokell/log-warper.git";
    sha256 = "0zwrjipjbjrqlgwdyn8ad0m28bndgz1dr2z01k4464c1whcy46m1";
    rev = "b3b2d4d5afa2c22f3aff10b4cceab3914c6bc0e5";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring containers directory dlist
    errors exceptions extra filepath formatting hashable lens
    monad-control monad-loops mtl network safecopy text text-format
    time transformers transformers-base universum unix
    unordered-containers yaml
  ];
  executableHaskellDepends = [ base exceptions text ];
  testHaskellDepends = [
    async base data-default directory filepath hspec HUnit lens
    QuickCheck universum unordered-containers
  ];
  homepage = "https://github.com/serokell/log-warper";
  description = "Flexible, configurable, monadic and pretty logging";
  license = stdenv.lib.licenses.mit;
}
