{ ... }:

let lib = import ../lib.nix; in
with lib;
with (import ../lib/ssh-keys.nix { inherit lib; });

let
  iohk-pkgs = import ../default.nix {};

in {
  network.description = "Mantis Dev Deployer";

  testnet-deployer = { config, pkgs, resources, ... }: {
    imports = [
      ../modules/common.nix
      ../modules/datadog.nix
      ../modules/papertrail.nix
      ../modules/deployer-base.nix
    ];

    services.dd-agent.tags = ["env:production" "depl:${config.deployment.name}" "role:deployer"];
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
    in [ list-developers ];
  };
}
