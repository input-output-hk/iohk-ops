with (import ./../lib.nix);

let
  accessKeyId = "iohk";
in {
  sl-explorer = { config, pkgs, resources, ... }: {
    imports = [ ./../modules/amazon-base.nix ];
    deployment.ec2 = {
      instanceType = mkForce "t2.large";
      ebsInitialRootDiskSize = mkForce 200;
      elasticIPv4 = resources.elasticIPs.sl-explorer-ip;
      associatePublicIpAddress = true;
      inherit accessKeyId;
      keyPair = resources.ec2KeyPairs.iohk;
    };
  };

  resources = {
    inherit ec2KeyPairs;
    elasticIPs = {
      sl-explorer-ip = { inherit region accessKeyId; };
    };
  };
}
