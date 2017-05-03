with (import ./../lib.nix);

let
  timeWarpNode = region: keypair: { pkgs, resources, ... }: {
    imports = [ ./../modules/amazon-base.nix ];

    deployment.ec2.region = mkForce region;
    deployment.ec2.keyPair = mkForce resources.ec2KeyPairs."${keypair}";
  };
in {
  network.description = "Time-warp experiments";

  timewarp0 = timeWarpNode "eu-central-1" "cardano-test-eu-central";
  timewarp1 = timeWarpNode "eu-central-1" "cardano-test-eu-central";
  timewarp2 = timeWarpNode "eu-central-1" "cardano-test-eu-central";
  timewarp3 = timeWarpNode "eu-central-1" "cardano-test-eu-central";
  timewarp4 = timeWarpNode "eu-central-1" "cardano-test-eu-central";
}
