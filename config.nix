# These constants are all used by runexperiment

# The rest are now configured centrally in modules/cardano-service.nix

rec {
  txgenR = 1;
  txgenN = 3;
  txgenPause = 0;
  txgenInitTps = 5;
  txgenTpsStep = 5;
  txgenP = 2;
  txgenAddresses = [ 0 ];
  #txgenMofN = [ 3 5 ];

  # Delegation settings (used for runexperiment)
  enableDelegation = false;
  delegationNode = 0;
}
