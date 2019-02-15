{
  network.description = "Deployer";

  deployer.imports = [
    ../modules/common.nix
    ../modules/deployer-base.nix
    ../modules/deployer-aws.nix
  ];
}
