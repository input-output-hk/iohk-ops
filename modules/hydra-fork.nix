{ fetchFromGitHub, nixpkgsPath }:

let
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "hydra";
    rev = "b57e864168651db9f5982a3ae31a91b0affbe40d";
    sha256 = "0j445iidw32ddgyk4mmjncwwh5mmfbycsr87x6havpwijb3jgjrr";
  };

  hydraRelease = (import (src + "/release.nix") {
    nixpkgs = nixpkgsPath;
    hydraSrc = {
      outPath = src;
      rev = builtins.substring 0 6 src.rev;
      revCount = 1234;
    };
  });

in
  hydraRelease.build.x86_64-linux
