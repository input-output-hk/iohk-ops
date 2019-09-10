{ config, lib, ... }:

with lib;

let
  commonLib = import ../lib.nix;
  cfg = config.services.hydra-slave;
in
{
  imports = [ ./auto-gc.nix
              ./nix_nsswitch.nix
            ];

  options = {
    services.hydra-slave = {
      cores = mkOption {
        type = mkOptionType {
          name = "hydra-slave-cores";
          check = t: isInt t && t >= 0;
        };
        default = 0;
        description = ''
          The number of slave cores to utilize per build job.
          0 is defined as unlimited.  0 is the default.
        '';
      };
    };
  };

  config = {
    nix = {
      buildCores = mkForce cfg.cores;
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
      commonLib.ciInfraKeys ++
      map (key: ''
        command="nice -n20 nix-store --serve --write" ${key}
      '') commonLib.buildSlaveKeys.linux;
  };
}
