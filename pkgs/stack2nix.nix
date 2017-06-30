{ mkDerivation, async, base, bytestring, Cabal, containers
, data-fix, directory, fetchgit, filepath, Glob, hnix
, monad-parallel, optparse-applicative, process, SafeSemaphore
, stdenv, temporary, text, yaml
}:
mkDerivation {
  pname = "stack2nix";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack2nix.git";
    sha256 = "076rsasis3vxbfrp5r3aar4yz9qcrawy8hysvbix62823w12lfas";
    rev = "3f2d4d31b1936dda71ce9a3be145c6407685045e";
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
