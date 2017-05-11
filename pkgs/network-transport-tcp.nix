{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network, network-transport, network-transport-tests
, stdenv, uuid
}:
mkDerivation {
  pname = "network-transport-tcp";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/serokell/network-transport-tcp.git";
    sha256 = "1176y7dqyk7gn4576zxy7l03l7h4jsq9xw4bpgbm3qfpax6mfimf";
    rev = "a6c04c35f3a1d786bc5e57fd04cf3e2a043179f3";
  };
  libraryHaskellDepends = [
    base bytestring containers data-accessor network network-transport
    uuid
  ];
  testHaskellDepends = [
    base network network-transport network-transport-tests
  ];
  doCheck = false;
  homepage = "http://haskell-distributed.github.com";
  description = "TCP instantiation of Network.Transport";
  license = stdenv.lib.licenses.bsd3;
}
