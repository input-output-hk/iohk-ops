rec {
  # Compile time settings
  genesisN = 6;
  slotDuration = 20;
  networkDiameter = 15;
  mpcRelayInterval = 45;

  # Run time settings
  bitcoinOverFlat = false;
  totalMoneyAmount = 60000000;
  nodePort = 3000;
  enableP2P = false;
  # enableP2P = true;

  txgenR = 1;
  txgenN = 20;
  txgenPause = 20;
  txgenInitTps = 1;
  txgenTpsStep = 5;
  txgenP = 2;
  txgenAddresses = [ 0 ];
  #txgenMofN = [ 3 5 ];

  # Delegation settings
  enableDelegation = false;
  #enableDelegation = true;
  delegationNode = 0;

  productionMode = false;
  systemStart = 0;
}
