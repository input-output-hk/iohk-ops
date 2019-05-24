{ globals, ... }:

let

  mkRule = { org, region }: {
    name = "allow-deployer-ssh-${region}-${org}";
    value = {
      inherit region;
      _file = ./allow-deployer-ssh.nix;
      accessKeyId = globals.orgAccessKeys.${org};
      description = "SSH";
      rules = [
        {
          protocol = "tcp"; # TCP
          fromPort = 22;
          toPort = 22;
          sourceIp = globals.deployerIP + "/32";
        }
      ];
    };
  };
in {
  resources.ec2SecurityGroups = builtins.listToAttrs (map mkRule globals.orgXRegions);
}
