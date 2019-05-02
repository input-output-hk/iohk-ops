with builtins;
with import ../../lib.nix;

let
  mkExplorer = mantisNodeName: {
    imports = [ <module/explorer-remix-solidity-services.nix> ];
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

  mantis-a-0 = mkMantis;
  mantis-a-1 = mkMantis;
  mantis-b-0 = mkMantis;
  mantis-b-1 = mkMantis;
  mantis-c-0 = mkMantis;
}
