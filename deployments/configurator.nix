{ configFile ? <config>
, accessKeyId
, deployerIP
, ...
}:
with (import <nixpkgs> {}).lib;
with import configFile; {

  ## Per-machine defaults:
  defaults = {
    imports = [ <module/parameters.nix> ];
    node    = { inherit accessKeyId org region; };
    cluster = { inherit deployerIP hostedZone; };
  };

  ## Universal resource logic:
  resources.route53HostedZones = optionalAttrs (hostedZone != null) {
    hostedZone = { config, ... }: {
      inherit region accessKeyId;
      name = "${hostedZone}";
    };
  };

  resources.ec2KeyPairs."cardano-keypair-${org}-${region}" = {
    inherit accessKeyId region;
  };

  resources.ec2SecurityGroups = {
    "allow-deployer-ssh-${region}-${org}" = {
      inherit region accessKeyId;
      description = "SSH";
      rules = [{
        protocol = "tcp"; # TCP
        fromPort = 22; toPort = 22;
        sourceIp = deployerIP + "/32";
      }];
    };
  };
}
