{ stdenv
, autoreconfHook
, getSrc
}:

stdenv.mkDerivation rec {
  name = "secp256k1";
  src  = getSrc name;

  buildInputs = [ autoreconfHook ];

  configurePhase = ''
    ./configure --enable-module-recovery --enable-module-ecdh --enable-experimental --prefix $lib
  '';

  doCheck = true;
  checkPhase = "./tests";

  outputs = [ "out" "lib" ];
}
