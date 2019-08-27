{ pkgs }:

let
  hydraSrc = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "hydra";
    rev = "906d2493c5c74673ba317d0b5ce7d97b062bd39b";
    sha256 = "0i7szp04c873gfmj1h0dcl5rsbzzldc160pcls8z9v6iphils34i";
  };
in
  pkgs.callPackage ./hydra-fork.nix {
      nixpkgsPath = pkgs.path;
      #patches = [
      #  (pkgs.fetchpatch {
      #    url = "https://github.com/NixOS/hydra/pull/648/commits/4171ab4c4fd576c516dc03ba64d1c7945f769af0.patch";
      #    sha256 = "1fxa2459kdws6qc419dv4084c1ssmys7kqg4ic7n643kybamsgrx";
      #  })
      #];
      src = hydraSrc;
    }
