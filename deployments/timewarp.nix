with (import ./../lib.nix);

let
  timeWarpReceiver = region: keypair: testIndex: { pkgs, resources, ... }: {
    imports = [ ./../modules/timewarp-node.nix ];
    services.timewarp-node.enable = true;
    networking.firewall.enable = mkForce false;
  } // optionalAttrs (generatingAMI == false) {
    deployment.ec2.region = mkForce region;
    deployment.ec2.keyPair = mkForce (keypair resources.ec2KeyPairs);
  };
  timeWarpSender = region: keypair: testIndex: { pkgs, resources, ... }: {
    imports = [ (timeWarpReceiver region keypair testIndex) ];
    services.timewarp-node.sender = true;
  };
  snd-node-eu = timeWarpSender "eu-central-1" (pairs: pairs.cardano-test-eu);
  rcv-node-eu = timeWarpReceiver "eu-central-1" (pairs: pairs.cardano-test-eu);
  rcv-node-us = timeWarpReceiver "us-west-1" (pairs: pairs.cardano-test-us);
  rcv-node-asia = timeWarpReceiver "ap-southeast-1" (pairs: pairs.cardano-test-asia);
  rcv-node-sydney = timeWarpReceiver "ap-southeast-2" (pairs: pairs.cardano-test-sydney);
  rcv-node-sa = timeWarpReceiver "sa-east-1" (pairs: pairs.cardano-test-sa);
in 
  (genAttrs' (range 0 0) (key: "timewarp${toString key}") (name: snd-node-eu name)) // 
  (genAttrs' (range 1 2) (key: "timewarp${toString key}") (name: rcv-node-eu name)) // 
  (genAttrs' (range 3 4) (key: "timewarp${toString key}") (name: rcv-node-eu name)) // 
{
  network.description = "Time-warp experiments";

  resources = {
    inherit ec2KeyPairs;
  };
}
