builtins.trace "[WARN] deployments/configurator.nix has been split into deployments/gac/configurator.nix and deployments/gac/common-aws.nix please update your symlinks" {
  require = [
    ./gac/configurator.nix
    ./gac/common-aws.nix
  ];
}
