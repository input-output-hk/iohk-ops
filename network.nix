{ nixpkgsSrc, depl }:
{
  inherit nixpkgsSrc depl;
  nixpkgs = import nixpkgsSrc {};
}
