let topology = import ./../../topology.nix; in
with builtins;
with import ../../lib.nix;

let
  mkExplorer = mantisNodeName: {
    imports = [ <module/exporer-remix-solidity-services.nix> ];
    config.services.mantis-explorer.mantis-node = mantisNodeName;
  };

  mkFaucet = mantisNodeName: {
    imports = [ <module/mantis-faucet-service.nix> ];
    config.services.mantis-faucet.mantis-node = mantisNodeName;
  };

  mkMantis = {
    imports = [ <module/mantis-service.pseudo.nix> ];
    config.services.mantis = {};
  };
in {
  network.description = "GMC";

  explorer-a = mkExplorer "mantis-a-0";

  faucet-a   = mkFaucet "mantis-a-0";
} // listToAttrs (map 
      (mantisNode: nameValuePair mantisNode mkMantis)
      (goguenNodes topology "mantis")
    )
