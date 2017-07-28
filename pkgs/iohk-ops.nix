{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-ec2
, amazonka-iam, base, bytestring, cassava, conduit, containers
, exceptions, lens, lens-aeson, mtl, optional-args, random, safe
, stdenv, system-filepath, text, text-format, time, tinylog, turtle
, unordered-containers, utf8-string, vector, yaml
}:
mkDerivation {
  pname = "iohk-ops";
  version = "0.1.0.0";
  src = ../iohk;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ec2 amazonka-iam base
    bytestring cassava conduit containers exceptions lens lens-aeson
    mtl optional-args random safe system-filepath text text-format time
    tinylog turtle unordered-containers utf8-string vector yaml
  ];
  doHaddock = false;
  doCheck = false;
  license = stdenv.lib.licenses.bsd3;
}
