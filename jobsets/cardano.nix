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
    iohk-ops-integration-test = [ "x86_64-linux" ];
    nixops = [ "x86_64-linux" ];
  });
  cardanoSrc = pkgs.fetchgit cardano-sl-src;
  cardano-sl-src = builtins.fromJSON (builtins.readFile ./../cardano-sl-src.json);
  cardanoRelease = import "${cardanoSrc}/release.nix" {
    cardano = {
      outPath = cardanoSrc;
      rev = cardano-sl-src.rev;
    };
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
    ci = (import ../nix-darwin/test.nix { role = "ci"; host = "build"; port = "123"; }).system;
  };
})
