{ lib, ... }:
with lib;
fix (self: let
  projects = self.packetSecrets.projects;
  accessKeyId = self.packetSecrets.accessKeyId;
in {
  mockCreds = {
    accessKeyId = "abc123";
    projects = {
      infra = {
        id = "1";
      };
      ci = {
        id = "2";
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
  mkPacketKeyPairs = projects: listToAttrs (map self.mkPacketKeyPair projects);
  mkPacketNet = { hostname, modules, type ? "demand", facility ? "any", plan ? "c1.small.x86", ipxeScriptUrl ? "", project ? "infra", extraopts ? {} }:
  { config, name, pkgs, resources, ... }: let
    projectId = projects.${project}.id;
  in {
    deployment = let
      deploymentTypes = {
        demand = {
          targetHost = hostname + ".aws.iohkdev.io";
          targetEnv = "packet";
          packet = {
            inherit accessKeyId facility plan ipxeScriptUrl;
            keyPair = resources.packetKeyPairs.${self.getPacketKeyPairName project};
            project = projectId;
          };
        };
        legacy = {
          targetHost = hostname + ".aws.iohkdev.io";
        };
      };
    in deploymentTypes.${type};

    imports = modules ++ [
                extraopts
              ] ++ (optionals (type == "legacy")
              (map (f: ./. + "/${hostname}/${f}")
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")))));
    environment.systemPackages = with pkgs;
      [ moreutils ethtool jq ];
    # services.extra-statsd = mkForce false;
  };
  createPacketHydraSlaveBuildkite = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    facility = "ams1";
    plan = "c2.medium.x86";
    project = "ci";
    ipxeScriptUrl = "http://173.61.28.54:9000/c2-medium-x86/netboot.ipxe";
    modules = [ 
      ../modules/hydra-slave.nix
      ../modules/buildkite-agent.nix
    ];
  };
  createPacketHydraSlave = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/hydra-slave.nix ];
  };
  createPacketBuildkite = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/buildkite-agent.nix ];
  };
  createPacketHydraSlaveImpure = hostname: self.mkPacketNet {
    inherit hostname;
    type = "demand";
    modules = [ ../modules/hydra-slave.nix ];
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketHydraSlaveImpureLegacy = hostname: mkPacketNet {
    inherit hostname;
    type = "legacy";
    modules = [ ../modules/hydra-slave.nix ];
    extraopts = { nix.useSandbox = mkForce false; };
  };
  createPacketMachines = function: hostname: { name = hostname; value = function hostname; };
})
