rec {
  # Compile time settings
  genesisN = 20;
  slotDuration = 20;
  networkDiameter = 15;
  mpcRelayInterval = 45;

  # Run time settings
  bitcoinOverFlat = false;
  totalMoneyAmount = 60000000;
  nodePort = 3000;
  enableP2P = true;

  txgenR = 5;
  txgenN = 3;
  txgenPause = 0;
  txgenInitTps = 1;
  txgenTpsStep = 0;
  txgenP = 2;
  txgenAddresses = [ 0 ];
  txgenMofN = [ 3 5 ];

  # Delegation settings
  enableDelegation = true;
  delegationNode = 0;
} 
