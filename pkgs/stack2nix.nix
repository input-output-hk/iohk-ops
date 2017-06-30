{ mkDerivation, async, base, bytestring, containers, data-fix
, directory, fetchgit, filepath, Glob, hnix, monad-parallel
, optparse-applicative, process, SafeSemaphore, stdenv, temporary
, text, yaml
}:
mkDerivation {
  pname = "stack2nix";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack2nix.git";
    sha256 = "01b8kc205m6p2q3l4an0gx9062w9szjkk139m3qnra7g4bh5551g";
    rev = "59ee4de0223da8ad8ae56adb02f39ec365a20d42";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers data-fix directory filepath Glob
    hnix monad-parallel process SafeSemaphore temporary text yaml
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  doCheck = false;
  description = "Convert stack.yaml files into Nix build instructions.";
  license = stdenv.lib.licenses.bsd3;
}
