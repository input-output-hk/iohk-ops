{ ... }:

let
  iohk-pkgs = import ../default.nix {};
in {
  environment.systemPackages = [ iohk-pkgs.nixops ];
}
