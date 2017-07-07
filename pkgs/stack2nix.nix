{ mkDerivation, async, base, bytestring, Cabal, containers
, data-fix, directory, fetchgit, filepath, Glob, hnix
, monad-parallel, optparse-applicative, process, SafeSemaphore
, stdenv, temporary, text, yaml
}:
mkDerivation {
  pname = "stack2nix";
  version = "0.1.3.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack2nix.git";
    sha256 = "10d83s7kg73fzycd18m0z4zvw8c844h21kdkfp2rl55fjp7gnf33";
    rev = "379b4daa5281dd65982ffe6a150fac11fd8cb901";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring Cabal containers data-fix directory filepath
    Glob hnix monad-parallel process SafeSemaphore temporary text yaml
  ];
  executableHaskellDepends = [ base Cabal optparse-applicative ];
  doCheck = false;
  description = "Convert stack.yaml files into Nix build instructions.";
  license = stdenv.lib.licenses.mit;
}
