{ mkDerivation, array, base, bytestring, cereal, containers
, directory, extensible-exceptions, fetchgit, filepath, mtl
, network, safecopy, stdenv, stm, template-haskell, th-expand-syns
, unix
}:
mkDerivation {
  pname = "acid-state";
  version = "0.14.2";
  src = fetchgit {
    url = "https://github.com/serokell/acid-state.git";
    sha256 = "109liqzk66cxkarw8r8jxh27n6qzdcha2xlhsj56xzyqc2aqjz15";
    rev = "95fce1dbada62020a0b2d6aa2dd7e88eadd7214b";
  };
  libraryHaskellDepends = [
    array base bytestring cereal containers directory
    extensible-exceptions filepath mtl network safecopy stm
    template-haskell th-expand-syns unix
  ];
  homepage = "http://acid-state.seize.it/";
  description = "Add ACID guarantees to any serializable Haskell data structure";
  license = stdenv.lib.licenses.publicDomain;
}
