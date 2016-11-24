with (import ./../lib.nix);

let
  timeWarpReceiver = { pkgs, resources, ... }: ({
    imports = [ ./../modules/timewarp-node.nix ];
    services.timewarp-node.enable = true;
  } // optionalAttrs (generatingAMI != "1") {
    deployment.ec2.region = "eu-central-1";
    deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
    deployment.ec2.ami = (import ./../modules/amis.nix)."eu-central-1";
  });
  timeWarpSender = {
    imports = [ timeWarpReceiver ];
    services.timewarp-node.sender = true;
  };
in 
  (genAttrs' (range 1 5) (key: "timewarp${toString key}") (name: timeWarpReceiver)) // 
{
  timewarp0 = timeWarpSender;

  resources.ec2KeyPairs.cardano-test-eu = {
    accessKeyId = "cardano-deployer";
    region = "eu-central-1";
  };
}
