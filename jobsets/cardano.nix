let
  fixedNixpkgs = (import ../lib.nix).nixpkgs;
in { pkgs ? (import fixedNixpkgs {})
   , supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
   , scrubJobs ? true
   }:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { inherit supportedSystems scrubJobs; packageSet = import ../.; };

let
  iohkpkgs = import ../. { inherit pkgs; };
  jobs = mapTestOn ((packagePlatforms iohkpkgs) // {
    iohk-ops = [ "x86_64-linux" ];
    github-webhook-util = [ "x86_64-linux" ];
    iohk-ops-integration-test = [ "x86_64-linux" ];
    nixops = [ "x86_64-linux" ];
  });
  cardanoSrc = (import ../nix/sources.nix).cardano-sl;
  cardanoRelease = import "${cardanoSrc}/release.nix" {
    cardano = cardanoSrc;
  };
  tests = import ../tests {
    inherit pkgs;
    supportedSystems = [ "x86_64-linux" ];
  };
in pkgs.lib.fix (jobsets: {
  inherit (jobs) iohk-ops iohk-ops-integration-test nixops;
  inherit (iohkpkgs) checks IFDPins cachecache;
  inherit tests;
  exchange-monitor = pkgs.callPackage ../modules/exchange-monitor {};
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "iohk-ops-required-checks";
    constituents =
      let
        all = x: map (system: x.${system}) supportedSystems;
    in [
      jobsets.iohk-ops.x86_64-linux
      (builtins.attrValues jobsets.tests)
      (builtins.attrValues jobsets.checks)
    ];
  });
  nix-darwin = {
    hydra-slave = (import ../nix-darwin/test.nix { role = "hydra-slave"; }).system;
    buildkite-agent = (import ../nix-darwin/test.nix { role = "buildkite-agent"; }).system;
  };
})
