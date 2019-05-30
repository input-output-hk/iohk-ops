{ globals, ... }:

let

  mkRule = { org, region }: {
    name = "allow-all-${region}-${org}";
    value = {
      inherit region;
      _file = ./allow-all.nix;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "Allow all ${org}/${region}";
      rules = [{
        protocol = "-1"; # all
        fromPort = 0; toPort = 65535;
        sourceIp = "0.0.0.0/0";
      }];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}
