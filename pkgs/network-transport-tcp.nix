{ mkDerivation, base, bytestring, containers, data-accessor
, fetchgit, network, network-transport, network-transport-tests
, stdenv, uuid
}:
mkDerivation {
  pname = "network-transport-tcp";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/avieth/network-transport-tcp.git";
    sha256 = "08zjqpcxlf044q02dhr6gx4ndzqvmimvwy49v4540rj2ah10gsf9";
    rev = "ca42a954a15792f5ea8dc203e56fac8175b99c33";
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
