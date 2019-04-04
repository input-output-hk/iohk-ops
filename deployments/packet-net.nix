{ ... }:

with (import ../lib.nix);
let
  mkPacketNet = hostname: module: extraopts: { config, name, pkgs, resources, ... }: {
    deployment.targetHost = hostname + ".aws.iohkdev.io";
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
    documentation.nixos.enable = false;
    # services.extra-statsd = mkForce false;
  } // extraopts;
in
{
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
}
