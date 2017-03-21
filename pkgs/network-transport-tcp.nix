{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network, network-transport, network-transport-tests
, stdenv
}:
mkDerivation {
  pname = "network-transport-tcp";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport-tcp.git";
    sha256 = "1lznyp62w4h5387p70i4hm29v52bi2cackxl4ig2adfi8dg83i88";
    rev = "1739cc6d5c73257201e5551088f4ba56d5ede15c";
  };
  libraryHaskellDepends = [
    base bytestring containers data-accessor network network-transport
  ];
  testHaskellDepends = [
    base network network-transport network-transport-tests
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com";
  description = "TCP instantiation of Network.Transport";
  license = stdenv.lib.licenses.bsd3;
}
