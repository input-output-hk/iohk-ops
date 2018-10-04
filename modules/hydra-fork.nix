{ fetchFromGitHub, nixpkgsPath }:

let
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "hydra";
    sha256 = "0wnhlq5vi6hkcizicflgcf75a5qw84a5950kkvay0an2g7zb8n42";
    rev = "b57e864168651db9f5982a3ae31a91b0affbe40d";
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
