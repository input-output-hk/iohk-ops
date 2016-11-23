with (import ./../lib.nix);

let
  timeWarpReceiver = {
    imports = [ ./../modules/timewarp-node.nix ];
    services.timewarp-node = {
      enable = true;
    };
  } // optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = "eu-central-1";
    deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
  };
  timeWarpSender = timeWarpReceiver // {
    services.timewarp-node.sender = true;
  };
in 
  (genAttrs' (range 1 5) (key: "node${toString key}") (name: timeWarpReceiver)) // 
{
  node0 = timeWarpSender;

  resources.ec2KeyPairs.cardano-test-eu = {
    accessKeyId = "cardano-deployer";
    region = "eu-central-1";
  };
}
