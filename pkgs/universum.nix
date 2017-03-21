{ mkDerivation, base, bytestring, containers, deepseq, exceptions
, fetchgit, ghc-prim, hashable, microlens, microlens-mtl, mtl, safe
, stdenv, stm, text, text-format, transformers, type-operators
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "universum";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/serokell/universum";
    sha256 = "1cy1mkl2n9kjmya0225nc2bichb60q4y8n3wym7m79qcvsh6rxa2";
    rev = "9edb5d374ec1fb6bb873dacc22a013f1eacd9c67";
  };
  libraryHaskellDepends = [
    base bytestring containers deepseq exceptions ghc-prim hashable
    microlens microlens-mtl mtl safe stm text text-format transformers
    type-operators unordered-containers utf8-string vector
  ];
  doCheck = false;
  homepage = "https://github.com/serokell/universum";
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
}
