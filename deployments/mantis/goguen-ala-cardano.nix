with builtins;
with import ../../lib.nix;

let
  mkMantis = {
    imports = [ <module/mantis-service.pseudo.nix> ];
    config.services.mantis = {};
  };

  mkExplorer = mantisNodeName: {
    imports = [ <module/exporer-remix-solidity-services.nix> ];

    config.services.mantis-explorer = {
      mantis-node = mantisNodeName;
    };
  };
in {
  network.description = "GMC";

  mantis-a-0 = mkMantis;
  mantis-a-1 = mkMantis;
  mantis-b-0 = mkMantis;
  mantis-b-1 = mkMantis;
  mantis-c-0 = mkMantis;

  explorer-a = mkExplorer "mantis-a-0";
}
