{ ... }:

with (import ../lib.nix);
let
  mockCreds = {
    accessKeyId = "abc123";
    projects = {
      infra = {
        id = "1";
      };
    };
  };
  packetSecrets = if builtins.pathExists ../static/packet.nix then import ../static/packet.nix else mockCreds;
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
                extraopts
              ] ++ (optionals (type == "legacy")
              (map (f: ./. + "/${hostname}/${f}")
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
  createPacketBuildkite = { hostname, ... }@args: mkPacketNet ({
    inherit hostname;
    type = "demand";
    module = ../modules/buildkite-agent-container.nix;
  } // args);
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
  createPacketMachinesMod = function: { hostname, ... }@args: { name = hostname; value = function args; };
  hydraSlaves = [
    "packet-hydra-slave-1"
    "packet-hydra-slave-2"
    "packet-hydra-slave-3"
    "packet-hydra-slave-4"
    "packet-hydra-slave-5"

    # Temporary for extra load
    #"packet-hydra-slave-6"
    #"packet-hydra-slave-7"
    #"packet-hydra-slave-8"
    #"packet-hydra-slave-9"
    #"packet-hydra-slave-10"
  ];
  buildkiteAgents = [
    { hostname = "packet-buildkite-1";
      ipo4 = "11";
      extraopts = {
        services.buildkite-containers.hostIdSuffix = "1";
      };
    }
    { hostname = "packet-buildkite-2";
      ipo4 = "12";
      extraopts = {
        services.buildkite-containers.hostIdSuffix = "2";
      };
    }
    { hostname = "packet-buildkite-3";
      ipo4 = "13";
      extraopts = {
        services.buildkite-containers.hostIdSuffix = "3";
      };
    }

    # Temporary for extra load
    #{ hostname = "packet-buildkite-4";
    #  ipo4 = "14";
    #  extraopts = {
    #    services.buildkite-containers.hostIdSuffix = "4";
    #  };
    #}
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
    (map (createPacketMachinesMod createPacketBuildkite) buildkiteAgents)
   ])
