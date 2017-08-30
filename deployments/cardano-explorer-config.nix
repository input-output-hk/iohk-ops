{ accessKeyId, deployerIP, environment, systemStart }:

with (import ./../lib.nix);
let
  config     = import ./cardano-nodes-config.nix { inherit accessKeyId deployerIP systemStart environment; };
  spec       = {
    region   = "eu-central-1";
    type     = "other";
    kademlia = false;
    peers    = [];
  };
in spec // rec {
        inherit (config.globalArgs) allNames environment firstRelayIndex nRelays systemStart;
              i = length allNames; # assign last index
     relayIndex = null;
           name = "explorer";
                  ## For the SG definitions look in 'deployments/cardano-nodes-config.nix'
        sgNames = [ "allow-deployer-ssh-${region}"
                    "allow-to-explorer-${region}" ];
}
