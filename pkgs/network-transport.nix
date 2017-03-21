{ mkDerivation, base, binary, bytestring, deepseq, fetchgit
, hashable, stdenv, transformers
}:
mkDerivation {
  pname = "network-transport";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport.git";
    sha256 = "0gd1c36khb0mdp0w9b8xrmcr1pdnki2l9m2rck8y5lmn5xjf2bhr";
    rev = "f2321a103f53f51d36c99383132e3ffa3ef1c401";
  };
  libraryHaskellDepends = [
    base binary bytestring deepseq hashable transformers
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com";
  description = "Network abstraction layer";
  license = stdenv.lib.licenses.bsd3;
}
