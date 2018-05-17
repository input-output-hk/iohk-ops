with import ./default.nix {};

pkgs.stdenv.mkDerivation {
  name = "iohk-ops-terraform";
  buildInputs = [ terraform pkgs.jq pkgs.awscli ];
  src = null;
}
