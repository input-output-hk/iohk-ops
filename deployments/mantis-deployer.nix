{ ... }:

let lib = import ../lib.nix; in
with lib;
with import ../lib/ssh-keys.nix { inherit lib; };

let
  iohk-pkgs = import ../default.nix {};

in {
  network.description = "Mantis Dev Deployer";

  testnet-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/common.nix
      ../modules/monitoring-exporters.nix
      ../modules/deployer-base.nix
    ];

    nix.binaryCaches = mkForce [
      "https://cache.nixos.org"
      "https://hydra.iohk.io" "https://mantis-hydra.aws.iohkdev.io"
    ];
    nix.binaryCachePublicKeys = mkForce [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra.iohk.io-1:E8yDJv2SBXM6PQPVbhCWK7VvitistFYSH2u3AuwCiu4="
    ];

    networking.hostName = "devMantis-deployer";

    users.groups.developers = {};

    users.users = {
      # Deploy cardano-sl testnet
      dev = {
        isNormalUser = true;
        description  = "Mantis Developer";
        group        = "deployers";
        openssh.authorizedKeys.keys = devOpsKeys ++ mantisOpsKeys ++ allKeysFrom csl-developers;
      };
    }
    # Normal users who can deploy developer clusters on AWS.
    // mapAttrs (name: keys: {
      group = "developers";
      isNormalUser = true;
      openssh.authorizedKeys.keys = keys;
    }) (csl-developers // devOps // mantis-devOps);

    environment.systemPackages = let
      usernames = attrNames (csl-developers // devOps // mantis-devOps);
      list-developers = pkgs.writeShellScriptBin "list-developers"
        (concatMapStringsSep "\n" (u: "echo ${u}") usernames);
    in [ list-developers ] ++ ( with iohk-pkgs; [ terraform mfa ]);
  };
}
