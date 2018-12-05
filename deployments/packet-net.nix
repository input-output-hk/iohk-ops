{ ... }:

with (import ../lib.nix);
let
  hydra-agents-projid = "b927c2aa-a669-4497-af71-bd46a8cba4d8";
  accessKeyId = builtins.readFile ../static/packet-accessKeyId.secret;
  mkPacketNet = hostname: module: extraopts: { config, name, pkgs, resources, ... }: {
    deployment.targetHost = hostname + ".aws.iohkdev.io";
    services.dd-agent.tags = ["group:linux"];
    imports = [ ../modules/common.nix
                module
              ] ++
              # This loads the packet.net-provided .nix files.
              map (f: builtins.toPath(toString(./.) + "/${hostname}/" + f))
                  (builtins.attrNames (builtins.readDir (./. + "/${hostname}")));
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
    ## Disabled due to build failures:
    services.nixosManual.enable = false;
    services.extra-statsd = mkForce false;
  } // extraopts;
in
{
  # to be retired and relaunched using nixops packet.net module at some point
  builder-packet-c1-small-x86   = mkPacketNet "builder-packet-c1-small-x86"   ../modules/hydra-slave.nix     {};
  builder-packet-c1-small-x86-2 = mkPacketNet "builder-packet-c1-small-x86-2" ../modules/hydra-slave.nix     {};
  builder-packet-c1-small-x86-3 = mkPacketNet "builder-packet-c1-small-x86-3" ../modules/hydra-slave.nix     {};
  builder-packet-c1-small-x86-4 = mkPacketNet "builder-packet-c1-small-x86-4" ../modules/hydra-slave.nix     {};
  builder-packet-c1-small-x86-5 = mkPacketNet "builder-packet-c1-small-x86-5" ../modules/hydra-slave.nix     {};

  mantis-slave-packet-1         = mkPacketNet "mantis-slave-packet-1"         ../modules/hydra-slave.nix     { nix.useSandbox = mkForce false; };
  mantis-slave-packet-2         = mkPacketNet "mantis-slave-packet-2"         ../modules/hydra-slave.nix     { nix.useSandbox = mkForce false; };

  buildkite-packet-1            = mkPacketNet "buildkite-packet-1"            ../modules/buildkite-agent.nix {};
  buildkite-packet-2            = mkPacketNet "buildkite-packet-2"            ../modules/buildkite-agent.nix {};
  buildkite-packet-3            = mkPacketNet "buildkite-packet-3"            ../modules/buildkite-agent.nix {};

  # New instances
  resources.packetKeyPairs.infra = {
    inherit accessKeyId;
    project-id = hydra-agents-projid;
  };

  buildkite-packet-4 = { resources, ... }: {
    deployment.targetEnv = "packet";
    deployment.packet = {
      inherit accessKeyId;
      keyPair = resources.packetKeyPairs.infra;
      facility = "ams1";
      plan = "c1.small.x86";
      project-id = hydra-agents-projid;
    };
  };
  buildkite-packet-5 = { resources, ... }: {
    deployment.targetEnv = "packet";
    deployment.packet = {
      inherit accessKeyId;
      keyPair = resources.packetKeyPairs.infra;
      facility = "ams1";
      plan = "c1.small.x86";
      project-id = hydra-agents-projid;
    };
  };
  buildkite-packet-6 = { resources, ... }: {
    deployment.targetEnv = "packet";
    deployment.packet = {
      inherit accessKeyId;
      keyPair = resources.packetKeyPairs.infra;
      facility = "ams1";
      plan = "c1.small.x86";
      project-id = hydra-agents-projid;
    };
  };
}
