{ ... }@args:
let
  ## fetch-nixpkgs.nix uses nixpkgs-src.json from its directory,
  ## ..so we have a Goguen-specific pair of those.
  nixpkgsSrc = (import ../lib.nix).goguenNixpkgs;
  nixpkgs    = import (builtins.trace "nixpkgs=${nixpkgsSrc}" nixpkgsSrc) {};
  goguenPkgs = import ./. (args // { pkgs = nixpkgs; });
in goguenPkgs
