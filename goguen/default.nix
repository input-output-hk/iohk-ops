with builtins;
{ pkgs ? import (import ../lib.nix).fetchNixPkgs {}
## Hack to make mantis-cardano parametrisable both in Hydra and in a local evaluation: 
, valPath         ? path: key: type: (path + "/${key}.${type}")
, interpSrcJsonAt ? path: repo: pkgs.fetchgit (removeAttrs (fromJSON (readFile (valPath path repo "src-json"))) ["date"])
, mantis-cardano  ? interpSrcJsonAt ./pins "mantis-cardano"
, ...
}@args:
with pkgs.lib; with pkgs;
let
  listVersions = drvs: stdenv.mkDerivation {
    name = "versions";
    src = ./.;
    buildInputs = [ git ];
    installPhase =
      concatStringsSep "\n"
        (
          [ "mkdir $out" ]
          ++
          (map
            (drv: ''
                (
                  cd ${drv.src}
                  SHA=`git rev-parse HEAD || echo UNKNOWN`
                  printf "%40s %s\n" $SHA ${drv.name}
                ) >> $out/versions.txt
             '')
             drvs)
        );
  };
  ## getSrc: if we have an input -- use it, if not, get a fetchgit-style pin.
  getSrc          = name:
    if hasAttr "${name}" args
    then args.${name}
    else interpSrcJsonAt ./pins name;
in
rec {
  iele            = callPackage ./iele.nix             { inherit getSrc secp256k1;      };
  kevm            = callPackage ./kevm.nix             { inherit getSrc;                };
  mantis          = callPackage ./mantis.nix           { inherit getSrc sbtVerify; mantis-cardano = mantis-cardano; };
  remixIde        = callPackage ./remix-ide.nix        { inherit getSrc trimmedSolcBin; };
  sbtVerify       = callPackage ./sbt-verify.nix       { inherit getSrc;                };
  secp256k1       = callPackage ./secp256k1.nix        { inherit getSrc;                };
  solidity        = callPackage ./solidity.nix         { inherit getSrc iele;           };
  solidityService = callPackage ./solidity-service.nix { inherit getSrc solidity iele;  };
  trimmedSolcBin  = callPackage ./trimmed-solc-bin.nix { inherit getSrc;                };

  versions        = listVersions [
    iele
    kevm
    mantis
    remixIde
    sbtVerify
    secp256k1
    solidity
    solidityService
  ];
}
