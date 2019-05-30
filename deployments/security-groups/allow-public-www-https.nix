{ globals, ... }:

let

  mkRule = { org, region }: {
    name = "allow-public-www-https-${region}-${org}";
    value = {
      _file = ./allow-public-www-https.nix;
      inherit region;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "WWW-http(s)";
      rules = [
        {
          protocol = "tcp";
          fromPort = 80; toPort = 80;
          sourceIp = "0.0.0.0/0";
        }
        {
          protocol = "tcp";
          fromPort = 443; toPort = 443;
          sourceIp = "0.0.0.0/0";
        }
      ];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}

