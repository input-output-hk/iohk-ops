{ mkDerivation, async, base, bytestring, Cabal, containers
, data-fix, directory, fetchgit, filepath, Glob, hnix
, monad-parallel, optparse-applicative, path, process
, SafeSemaphore, stack, stdenv, temporary, text, yaml
}:
mkDerivation {
  pname = "stack2nix";
  version = "0.1.3.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack2nix.git";
    sha256 = "1sdrsqc0xz7xn4knf47qaj3xxs9jns8izcqnx6cn84gpv76hhiqf";
    rev = "05420f3efb38834700a3da8a5245fecc50b01139";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring Cabal containers data-fix directory filepath
    Glob hnix monad-parallel path process SafeSemaphore stack temporary
    text yaml
  ];
  executableHaskellDepends = [ base Cabal optparse-applicative ];
  doCheck = false;
  description = "Convert stack.yaml files into Nix build instructions.";
  license = stdenv.lib.licenses.mit;
}
