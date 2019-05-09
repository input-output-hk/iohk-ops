{ stdenv
, getSrc
, scala
, sbt
}:

stdenv.mkDerivation rec {
  name = "sbt-verify";
  src  = getSrc name;

  buildInputs = [ scala sbt ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    export "_JAVA_OPTIONS=-Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy"
  '';

  buildPhase = "
    sbt publishLocal
  ";

  installPhase = ''
    mkdir $out
    cp -r .ivy .sbt target $out
  '';
}
