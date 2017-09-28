{ accessKeyId, deployerIP, systemStart, topologyYaml }:

with (import ./../lib.nix);
let
  mkNode = index: {
    name = "edgenode-${toString index}";
    value = {
      imports = [ (import ../modules/cardano-edgenode.nix { region = "eu-central-1"; inherit accessKeyId systemStart; }) ];
    };
  };
in listToAttrs (map mkNode (range 1 1))
