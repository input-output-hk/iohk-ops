with builtins; with import ../lib.nix;
{ pkgs ? import goguenNixpkgs {}
, ...
}:
with pkgs.lib; with pkgs;
let
  listVersions = drvs: runCommand {
    name = "versions";
    buildInputs = [ git ];
  } (concatStringsSep "\n"
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
      )
    );

  getSrc          = name: fetchPinAuto ./pins name;
  getPin          = name: readPin      ./pins name; ## To allo package reference their own pins, for e.g. Git commit ids.
  buildSbtLib     = (import ./build-sbt-lib.nix { inherit stdenv scala sbt sbt-extras; }).buildSbtLib;
  fetchSbtDeps    = (import ./build-sbt-lib.nix { inherit stdenv scala sbt sbt-extras; }).fetchSbtDeps;
in
rec {
  docker-image         = callPackage ./docker-image.nix          { inherit mantis iele-semantics; };
  ethereum-explorer    = callPackage ./explorer.nix              { inherit getSrc;                };
  iele-semantics       = callPackage ./iele-semantics.nix        { inherit getSrc secp256k1;      };
  mantis               = callPackage ./mantis.nix                { inherit getSrc sbt-verify; };
  remix-ide            = callPackage ./remix-ide.nix             { inherit getSrc trimmed-solc-bin; };
  sbt-verify           = callPackage ./sbt-verify.nix            { inherit getSrc;                };
  secp256k1            = callPackage ./secp256k1.nix             { inherit getSrc;                };
  solidity             = callPackage ./solidity.nix              { inherit getSrc iele-semantics; };
  solidity-service     = callPackage ./solidity-service.nix      { inherit getSrc solidity iele-semantics; };
  trimmed-solc-bin     = callPackage ./trimmed-solc-bin.nix      { inherit getSrc;                };

  versions             = listVersions [
    iele-semantics
    mantis
    remix-ide
    sbt-verify
    secp256k1
    solidity
    solidity-service
  ];
}
