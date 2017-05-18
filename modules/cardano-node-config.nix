# TODO: get rid of this duplication between config.nix and modules/cardano-node.nix
with (import ./../lib.nix);

testIndex: region:
  { pkgs, ...}: {
    imports = [
      ./common.nix
    ];

    services.cardano-node = {
      enable = true;
      testIndex = testIndex;
      port = cconf.nodePort;
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval;
      inherit (cconf) totalMoneyAmount bitcoinOverFlat productionMode systemStart richPoorDistr;
    };
  }
