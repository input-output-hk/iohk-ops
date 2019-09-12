{ globals, config, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in

{
  faucet = if config.params.nodeImpl == "haskell" then {
    imports = [ ../modules/cardano-base.nix ../modules/cardano-faucet.nix ];
    params = nodeMap.faucet;
  } else
    if config.params.nodeImpl == "rust" then {
    imports = [
      (sources.jormungandr-nix + "/nixos")
      (sources.jormungandr-faucet + "/nix/nixos")
    ];
  } else {};
}
