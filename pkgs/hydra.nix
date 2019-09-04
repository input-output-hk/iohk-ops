{ pkgs }:

let
  hydraSrc = (import ../lib.nix).sources.hydra;
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
