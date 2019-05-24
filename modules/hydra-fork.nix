{ fetchFromGitHub, nixpkgsPath, src, patches }:

let
  hydraRelease = (import (src + "/release.nix") {
    nixpkgs = nixpkgsPath;
    hydraSrc = {
      outPath = src;
      rev = builtins.substring 0 6 src.rev;
      revCount = 1234;
    };
  });

in
  hydraRelease.build.x86_64-linux.overrideAttrs (drv: { inherit patches; })
