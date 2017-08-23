# Use as follows:
#
#  nix-build iohk/iohk-env.nix
#  alias io=/nix/store/604hqdwlw82pn19s5aszmxjkrjfadgqn-ghc-8.0.2-with-packages/bin/runhaskell -i/home/staging/iohk/iohk ~/iohk/iohk/iohk-ops.hs
#
let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, compiler ? pkgs.haskell.packages.ghc802
}:
{
  iohk-env = compiler.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    aeson aeson-pretty base bytestring cassava containers dns hourglass
    lens lens-aeson mtl optional-args safe system-filepath text turtle
    unordered-containers utf8-string vector yaml
  ]);
}
