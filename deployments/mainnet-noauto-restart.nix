{ environment, globals, ... }:

let
  noRestartNodes = [ "r-a-1" ];
  filteredList = builtins.filter (name: globals.nodeMap ? "${name}") noRestartNodes;
  f = name: { inherit name; value = { systemd.services.cardano-restart.enable = false; }; };
  deployment = builtins.listToAttrs (map f filteredList);
in if environment == "production" then deployment else {}
