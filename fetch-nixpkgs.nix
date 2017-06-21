(import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))
