
with import ../lib.nix;
with import ./packet-lib.nix { inherit lib; };

{
  defaults = {
    imports = [
      ../modules/common.nix
      ../modules/globals.nix
    ];
  };

  hydra = mkPacketNet {
    hostname = "hydra";
    type = "demand";
    module = ../modules/hydra-master-common.nix;
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    extraopts = {
    };
  };
}
