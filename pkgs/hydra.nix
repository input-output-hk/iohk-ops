{ pkgs }:

let
  hydraSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "hydra";
    rev = "0768891e3cd3ef067d28742098f1dea8462fca75";
    sha256 = "1aw3p7jm2gsakdqqx4pzhkfx12hh1nxk3wkabcvml5ci814f6jic";
  };
in
  pkgs.callPackage ./hydra-fork.nix {
      nixpkgsPath = pkgs.path;
      patches = [
        (pkgs.fetchpatch {
          url = "https://github.com/NixOS/hydra/pull/648/commits/4171ab4c4fd576c516dc03ba64d1c7945f769af0.patch";
          sha256 = "1fxa2459kdws6qc419dv4084c1ssmys7kqg4ic7n643kybamsgrx";
        })
      ];
      src = hydraSrc;
    }
