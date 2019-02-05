{ pkgs
, iele
, getSrc
}:

pkgs.stdenv.mkDerivation rec {
  name = "solidity";
  src  = getSrc name;

  buildInputs = with pkgs; [
    cmake
    llvm
    (boost.override { enableStatic = true; })
    libtool
    autoconf
    automake
    z3
    makeWrapper
  ];

  patches = [ ./solidity.patch ];

  configurePhase = ''
    export NIX_CFLAGS_COMPILE="-Wno-error=maybe-uninitialized $NIX_CFLAGS_COMPILE"
    mkdir build
    cd build
    cmake ..
  '';

  installPhase = ''
    mkdir -p $out/bin
    ls solc/
    mv solc/isolc $out/bin/
    wrapProgram $out/bin/isolc --prefix PATH : ${pkgs.lib.makeBinPath [ iele ]}
  '';
}
