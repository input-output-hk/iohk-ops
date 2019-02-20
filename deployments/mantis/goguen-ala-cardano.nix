with builtins;
with import <lib>;

let
  mkMantisMachine = vmType: nodeName: { nodes, resources, pkgs, config, ... }: {
    imports = [ <module/mantis-service.pseudo.nix> ];
    options = {
      mantis = mkOption {
        description = "Cluster parameters.";
        default = {};
        type = with types; submodule {
          options = {
            nodeNames = mkOption {
              type = listOf str;
              description = "List of Mantis node names.";
              default = [ "mantis-a-0" "mantis-a-1" "mantis-b-0" "mantis-b-1" "mantis-c-0" ];
            };
          };
        };
      };
    };

    config = {
      deployment.keys.mantis-node = {
        keyFile = <static> + "/${nodeName}.key";
        user = "mantis";
        destDir = "/var/lib/keys";
      };
      services.mantis = {
        inherit nodeName vmType;
      };
    };
  };
in {
  network.description = "GMC";

  mantis-a-0 = mkMantisMachine "iele" "mantis-a-0";
  mantis-a-1 = mkMantisMachine "iele" "mantis-a-1";
  mantis-b-0 = mkMantisMachine "iele" "mantis-b-0";
  mantis-b-1 = mkMantisMachine "iele" "mantis-b-1";
  mantis-c-0 = mkMantisMachine "iele" "mantis-c-0";
}
