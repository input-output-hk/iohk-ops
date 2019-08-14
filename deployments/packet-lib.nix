{lib, ... }:

lib.fix (self: let
  projects = self.packetSecrets.projects;
  accessKeyId = self.packetSecrets.accessKeyId;
in {
  mockCreds = {
    accessKeyId = "abc123";
    projects = {
      infra = {
        id = "1";
      };
    };
  };
  packetSecrets = if builtins.pathExists ../static/packet.nix then import ../static/packet.nix else self.mockCreds;
  getPacketKeyPairName = project: "packet-keypair-${project}";
  mkPacketKeyPair = project: {
    name = "packet-keypair-${project}";
    value = {
      inherit accessKeyId;
      project = projects.${project}.id;
    };
  };
  mkPacketKeyPairs = projects: lib.listToAttrs (map self.mkPacketKeyPair projects);
  mkPacketNet = { hostname, module, type ? "demand", facility ? "any", plan ? "c1.small.x86", project ? "infra", extraopts ? {} }:
  { config, name, pkgs, resources, ... }: let
    projectId = projects.${project}.id;
  in {
    deployment = let
      deploymentTypes = {
        demand = {
          targetHost = hostname + ".aws.iohkdev.io";
          targetEnv = "packet";
          packet = {
            inherit accessKeyId facility plan;
            keyPair = resources.packetKeyPairs.${self.getPacketKeyPairName project};
            project = projectId;
          };
        };
        legacy = {
          targetHost = hostname + ".aws.iohkdev.io";
        };
      };
    in deploymentTypes.${type};

    imports = [
                module
                extraopts
              ] ++ (optionals (type == "legacy")
              (map (f: ./. + "/${hostname}/${f}")
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")))));
    environment.systemPackages = with pkgs;
      [ moreutils ];
    # services.extra-statsd = mkForce false;
  };
  createPacketHydraSlave = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    module = ../modules/hydra-slave.nix;
  };
  createPacketBuildkite = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    module = ../modules/buildkite-agent.nix;
  };
  createPacketHydraSlaveImpure = hostname: self.mkPacketNet {
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
})
