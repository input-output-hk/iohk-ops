{ globals, ... }: with import ../lib.nix;
let nodeMap = { inherit (globals.fullMap) explorer; }; in

{
  explorer = { config, ... }:
  {
    imports = [
      ../modules/cardano-explorer.nix
      ../modules/cardano-explorer-python.nix
    ];
    params = nodeMap.explorer;
  };
}
