let
  fixedNixpkgs = (import ../lib.nix).fetchNixPkgs;
in {
  pkgs ? (import fixedNixpkgs {}), supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
}:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { inherit supportedSystems scrubJobs; packageSet = import ../.; };

let
  iohkpkgs = import ./../default.nix { inherit pkgs; };
  jobs = mapTestOn (packagePlatforms iohkpkgs);
in with pkgs; rec {
  inherit (jobs) cardano-report-server-static cardano-sl-static cardano-sl-explorer-static cardano-sl iohk-ops stack2nix;
  tests          = import ./../tests     { inherit pkgs; supportedSystems = [ "x86_64-linux" ]; };
}
