{ domain, ... }:

with import ../lib.nix;
with import ./packet-lib.nix { inherit domain; inherit (pkgs) lib; };
let
  hydraSlaves = [
    "packet-hydra-slave-1"
    "packet-hydra-slave-2"
    "packet-hydra-slave-3"
    "packet-hydra-slave-4"
    "packet-hydra-slave-5"
  ];
  buildkiteAgents = [
    "packet-buildkite-1"
    "packet-buildkite-2"
    "packet-buildkite-3"
  ];
  # Legacy systems, to be removed!
  mantisHydraLegacySlaves = [
    "mantis-slave-packet-1"
    "mantis-slave-packet-2"
  ];
in
{
  resources.packetKeyPairs = mkPacketKeyPairs [ "infra" ];
}
// builtins.listToAttrs (builtins.concatLists [
    (map (createPacketMachines createPacketHydraSlaveImpureLegacy) mantisHydraLegacySlaves)
    (map (createPacketMachines createPacketHydraSlave) hydraSlaves)
    (map (createPacketMachines createPacketBuildkite) buildkiteAgents)
   ])
