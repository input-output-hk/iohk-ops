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
    version = "1.10.0";
    sha256  = "0f1r7306m59pb3akvkninr46mcgjf09bvj2p0i050skr51a3vcb7";

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

in {
  inherit pkgs;
  terraform = pkgs.terraform_0_11.withPlugins (ps: [
    awsProvider
    ps.local
  ]);
}
