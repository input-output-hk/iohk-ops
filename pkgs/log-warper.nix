{ mkDerivation, aeson, ansi-terminal, async, base, containers
, data-default, directory, dlist, errors, exceptions, extra
, fetchgit, filepath, formatting, hashable, hspec, HUnit, lens
, mmorph, monad-control, monad-loops, mtl, network, QuickCheck
, safecopy, stdenv, text, text-format, time, transformers
, transformers-base, universum, unix, unordered-containers, yaml
}:
mkDerivation {
  pname = "log-warper";
  version = "1.1.1";
  src = fetchgit {
    url = "https://github.com/serokell/log-warper.git";
    sha256 = "06jc5dsg87xg0669dxdmmi0nxqnifdkilgcfmkcn2h3hwyda5ixc";
    rev = "c2a21fd2971d9ed48cdd7295f8efd56fc8ec4821";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base containers directory dlist errors
    exceptions extra filepath formatting hashable lens mmorph
    monad-control monad-loops mtl network safecopy text text-format
    time transformers transformers-base universum unix
    unordered-containers yaml
  ];
  executableHaskellDepends = [ base exceptions text universum yaml ];
  testHaskellDepends = [
    async base data-default directory filepath hspec HUnit lens
    QuickCheck universum unordered-containers
  ];
  homepage = "https://github.com/serokell/log-warper";
  description = "Flexible, configurable, monadic and pretty logging";
  license = stdenv.lib.licenses.mit;
}
