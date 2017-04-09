{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network, network-transport, network-transport-tests
, stdenv
}:
mkDerivation {
  pname = "network-transport-tcp";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport-tcp.git";
    sha256 = "0pklkrvvd38d7ykhclaz6rvn9wnn2gvpph25wbkkj2i990gz8q4k";
    rev = "15847920ac1a32ada237e5a00ac4f56f3ca421a7";
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
