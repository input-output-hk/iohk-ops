{ lib, config, ... }: with lib; with types; let
  cfg = config.configurator;
in {

  imports = [ <module/parameters.nix> <module/parameters-ops.nix> ];

  options.configurator = {
    optionDefaults = mkOption {
      description = "Option defaults";
      default = {};
      type = attrs;
    };
    userConfig = mkOption {
      description = "Cluster configuration";
      default = {};
      type = attrs;
    };
  };

  config = let
    optionDefaults = mapAttrsRecursive (_: mkOverride 1250) cfg.optionDefaults;
    userConfig     = mapAttrsRecursive (_: mkForce)         cfg.userConfig;
    mergedConfig   = builtins.deepSeq [optionDefaults userConfig ] recursiveUpdate optionDefaults userConfig;
  in {
      node    = mergedConfig.node;
      cluster = mergedConfig.cluster;
      hydra   = mergedConfig.hydra;
  };
}
