with (import ./../lib.nix);

let
  regions = [
    "eu-central-1"
    "eu-west-1"
    "eu-west-2"
    "ap-southeast-1"
    "ap-southeast-2"
    "ap-northeast-1"
    "ap-northeast-2"
  ];
  genSG = sgs: mergeAttrs (map sgs regions);
in {
  resources = {
    inherit ec2KeyPairs;
    ec2SecurityGroups = genSG (region: {
      /*"allow-cardano-${region}" = {
        inherit region accessKeyId;
        description = "Security group for cardano nodes";
        rules = [{
          fromPort = 22;
          toPort = 22;
          sourceIp = "0.0.0.0/0";
        }];
      };*/
      "allow-all-${region}" = {
        inherit region accessKeyId;
        description = "Catch all";
        rules = [{
          protocol = "-1"; # All traffic
          sourceIp = "0.0.0.0/0";
          fromPort = 0;
          toPort = 65535;
        }];
      };
    });
  };
}
