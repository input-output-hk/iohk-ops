{ configuration, system ? "x86_64-darwin" }:

let
  hostpkgs = import <nixpkgs> {};

  importJSON = file:
    let
      srcAttrs = builtins.fromJSON (builtins.readFile file);
      src = hostpkgs.fetchgit {
        inherit (srcAttrs) url rev sha256;
      };
    in import src;

  nixpkgs = importJSON ./nixpkgs.json {
    inherit system;
  };
  darwin = importJSON ./nix-darwin.json {
    nixpkgs = nixpkgs.path;
    inherit configuration system;
  };
in darwin
