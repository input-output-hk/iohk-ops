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
    sha256 = "0dldb74vq5wv0ijx4ih0y3nfaq4layjr0c63xk0c745z3pnsf0ky";
    rev = "8f82f6196695e382154547ad55638c3dfe0c17a3";
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
