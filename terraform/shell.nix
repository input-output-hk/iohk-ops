let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let
  iohk-pkgs = import ../default.nix { inherit system config pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "iohk-ops-terraform";
  buildInputs = [ iohk-pkgs.terraform pkgs.jq pkgs.awscli pkgs.keybase pkgs.gnupg ];
  src = null;
}
