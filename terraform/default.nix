let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let
  awsProvider = pkgs.buildGoPackage rec {
    owner   = "terraform-providers";
    repo    = "terraform-provider-aws";
    version = "1.19.0";
    sha256  = "14ap1gfhp04jcy0kwfghaqwm4ywm7zwqk3132iybmp2zx0rjf1np";

    name = "${repo}-${version}";
    goPackagePath = "github.com/${owner}/${repo}";
    src = pkgs.fetchFromGitHub {
      inherit owner repo sha256;
      rev = "v${version}";
    };

    # Terraform allow checking the provider versions, but this breaks
    # if the versions are not provided via file paths.
    postBuild = "mv go/bin/${repo}{,_v${version}}";
  };

in rec {
  terraform = pkgs.terraform_0_11.withPlugins (ps: [
    awsProvider
    ps.local
    ps.template
  ]);

  shell = pkgs.stdenv.mkDerivation {
    name = "iohk-ops-terraform";
    buildInputs = [ terraform pkgs.jq pkgs.awscli ];
    src = null;
  };
}
