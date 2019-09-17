{ globals, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; };
    params = nodeMap.faucet;
    sources = import ../nix/sources.nix;
    ifHaskell = pkgs.lib.optionals (params.nodeImpl == "haskell");
    ifRust = pkgs.lib.optionals (params.nodeImpl == "rust");
in

{
  faucet = {
    inherit params;

    imports = [ ../modules/cardano-base.nix  ] ++
      (ifHaskell [ ../modules/cardano-faucet.nix ]) ++
      (ifRust [
        ../modules/cardano.nix
        (sources.jormungandr-nix + "/nixos")
        (sources.jormungandr-faucet + "/nix/nixos")
      ]);
  };
}
