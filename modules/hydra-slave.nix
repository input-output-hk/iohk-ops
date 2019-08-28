{ lib, ... }:

with lib;

let
  localLib = import ../lib.nix;
in
{
  imports = [ ./auto-gc.nix
              ./nix_nsswitch.nix
            ];

  nix = {
    buildCores = mkForce 4;
    trustedBinaryCaches = mkForce [];
    binaryCaches = mkForce [ "https://cache.nixos.org" "https://iohk-nix-cache-temp.s3-eu-central-1.amazonaws.com/" ];
    binaryCachePublicKeys = mkForce [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io-1:pBSKwSWbeqlB7LnSdnxguemXjso7IVv6/h4W2joTycA="
    ];
    extraOptions = ''
      # Max of 2 hours to build any given derivation on Linux.
      # See ../nix-darwin/modules/basics.nix for macOS.
      timeout = ${toString (3600 * 2)}
    '';
  };

  users.extraUsers.root.openssh.authorizedKeys.keys =
    localLib.ciInfraKeys ++
    map (key: ''
      command="nice -n20 nix-store --serve --write" ${key}
    '') localLib.buildSlaveKeys.linux;
}
