{ globals, config, ... }: with (import ../lib.nix);
let nodeMap = { inherit (globals.fullMap) faucet; }; in

{
  faucet = {
    imports = [
      ../modules/cardano-base.nix
      ../modules/cardano-faucet.nix
      ../modules/cardano.nix
    ];
    params = nodeMap.faucet;
  };
}
