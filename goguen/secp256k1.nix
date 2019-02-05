{ stdenv
, autoconf
, automake
, libtool
, getSrc
}:

stdenv.mkDerivation rec {
  name = "secp256k1";
  src  = getSrc name;

  buildInputs = [ autoconf automake libtool ];

  configurePhase = ''
    ./autogen.sh
    ./configure --enable-module-recovery --enable-module-ecdh --enable-experimental --prefix $lib
  '';

  doCheck = true;
  checkPhase = "./tests";

  outputs = [ "out" "lib" ];
}
