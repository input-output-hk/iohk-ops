{ accessKeyId
, deployerIP
, config
, ...
}:
with (import <nixpkgs> {}).lib;
{
  ## Per-machine defaults:
  defaults = {
    # For Nixops configs, the `config` object we constructed in `/config.nix` should be sufficient;
    # for NixOS configs however, we need to make a distinction between defaults and and user-supplied parameters.
    imports = [ <module/configurator.nix> ];

    # The configurator *module* basically just changes the precedence of everything in `optionDefaults` and `userConfig` and merges them
    # together. The reason we have to send it off to a NixOS module is because we literally can't do it here in nixops. Trust, I tried. There be dragons and hydrae 🐉🐲.
    configurator = {
      # So yeah `config` already has both defaults and user-defined parameters but we can't tell which is which
      optionDefaults = config;
      # So we also pass the bare user config so that we can work the magic of set theory (I think?)
      userConfig     = import <config>;
    };
  };

  ## Universal resource logic:
  resources.ec2KeyPairs."cardano-keypair-${config.node.org}-${config.node.region}" = {
    inherit accessKeyId; inherit (config.node) region;
  };

  resources.ec2SecurityGroups = {
    "allow-deployer-ssh-${config.node.region}-${config.node.org}" = {
      inherit accessKeyId; inherit (config.node) region;
      description = "SSH";
      rules = [{
        protocol = "tcp"; # TCP
        fromPort = 22; toPort = 22;
        sourceIp = deployerIP + "/32";
      }];
    };
  };
}
