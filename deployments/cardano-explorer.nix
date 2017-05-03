with (import ./../lib.nix);

{
  network.description = "Cardano Explorer";

  sl-explorer = {
    imports = [
      (import ./../modules/cardano-node-config.nix 40 "")
      ./../modules/cardano-explorer.nix
    ];
  };
}
