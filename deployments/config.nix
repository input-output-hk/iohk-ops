{ accessKeyId, deployerIP, ... }:
let
  region = "eu-central-1";
  org    = "IOHK";
in {
  defaults = {
    imports = [ ../modules/parameters.nix ];
    node    = { inherit accessKeyId region org; };
    cluster = { inherit deployerIP; };
  };
}
