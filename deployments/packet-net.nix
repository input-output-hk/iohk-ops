{ ... }:

with (import ../lib.nix);
let
  packetSecrets = import ../static/packet.nix;
  projects = packetSecrets.projects;
  accessKeyId = packetSecrets.accessKeyId;
  getPacketKeyPairName = project: "packet-keypair-${project}";
  mkPacketKeyPair = project: {
    name = "packet-keypair-${project}";
    value = {
      inherit accessKeyId;
      project = projects.${project}.id;
    };
  };
  mkPacketKeyPairs = projects: pkgs.lib.listToAttrs (map mkPacketKeyPair projects);
  mkPacketNet = { hostname, module, type ? "demand", facility ? "any", plan ? "c1.small.x86", project ? "infra", extraopts ? {}, ...}: { config, name, pkgs, resources, ... }: let
    projectId = projects.${project}.id;
  in {
    deployment = let
      deploymentTypes = {
        demand = {
          targetHost = hostname + ".aws.iohkdev.io";
          targetEnv = "packet";
          packet = {
            inherit accessKeyId facility plan;
            keyPair = resources.packetKeyPairs.${getPacketKeyPairName project};
            project = projectId;
          };
        };
        legacy = {
          targetHost = hostname + ".aws.iohkdev.io";
        };
      };
    in deploymentTypes.${type};

    imports = [ ../modules/common.nix
                module
              ] ++ (optionals (type == "legacy")
              (map (f: builtins.toPath(toString(./.) + "/${hostname}/" + f))
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")))));
    environment.systemPackages = with pkgs;
      [ moreutils ];
    users = {
      users.staging = {
        description     = "cardano staging";
        group           = "staging";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.staging = {};
    };
    # services.extra-statsd = mkForce false;
  };
  createPacketHydraSlave = hostname: mkPacketNet {
    inherit hostname;
    type = "demand";
    module = ../modules/hydra-slave.nix;
  };
  createPacketHydraSlaveLegacy = hostname: mkPacketNet {
    inherit hostname;
    type = "legacy";
    module = ../modules/hydra-slave.nix;
  };
  createPacketBuildkite = hostname: mkPacketNet {
    inherit hostname;
    type = "demand";
    module = ../modules/buildkite-agent.nix;
  };
  createPacketBuildkiteLegacy = hostname: mkPacketNet {
    inherit hostname;
    type = "legacy";
    module = ../modules/buildkite-agent.nix;
  };
  createPacketHydraSlaveImpure = hostname: mkPacketNet {
    inherit hostname;
    type = "demand";
    module = ../modules/hydra-slave.nix;
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketHydraSlaveImpureLegacy = hostname: mkPacketNet {
    inherit hostname;
    type = "legacy";
    module = ../modules/hydra-slave.nix;
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketMachines = function: hostname: { name = hostname; value = function hostname; };
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
  hydraLegacySlaves = [
    "builder-packet-c1-small-x86"
    "builder-packet-c1-small-x86-2"
    "builder-packet-c1-small-x86-3"
    "builder-packet-c1-small-x86-4"
    "builder-packet-c1-small-x86-5"
  ];
  buildkiteLegacyAgents = [
    "buildkite-packet-1"
    "buildkite-packet-2"
    "buildkite-packet-3"
  ];
  mantisHydraLegacySlaves = [
    "mantis-slave-packet-1"
    "mantis-slave-packet-2"
  ];
in
{
  resources.packetKeyPairs = mkPacketKeyPairs [ "infra" ];
}
// builtins.listToAttrs (builtins.concatLists [
    (map (createPacketMachines createPacketHydraSlaveLegacy) hydraLegacySlaves)
    (map (createPacketMachines createPacketBuildkiteLegacy) buildkiteLegacyAgents)
    (map (createPacketMachines createPacketHydraSlaveImpureLegacy) mantisHydraLegacySlaves)
    (map (createPacketMachines createPacketHydraSlave) hydraSlaves)
    (map (createPacketMachines createPacketBuildkite) buildkiteAgents)
   ])
