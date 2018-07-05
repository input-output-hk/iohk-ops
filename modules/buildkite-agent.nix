{ name, pkgs, ... }:

{
  imports = [
    ./auto-gc.nix
    ./nix_nsswitch.nix
    ./docker-builder.nix
  ];

  services.buildkite-agent = {
    enable = true;
    name   = name;
    openssh.privateKeyPath = "/run/keys/buildkite-ssh-private";
    openssh.publicKeyPath  = "/run/keys/buildkite-ssh-public";
    tokenPath              = "/run/keys/buildkite-token";
    meta-data              = "system=x86_64-linux";
    runtimePackages        = with pkgs; [ bash gnutar gzip bzip2 xz nix ];
    hooks.environment = ''
      export NIX_BUILD_SHELL="/run/current-system/sw/bin/bash"
      export PATH="/run/current-system/sw/bin:$PATH"

      # load S3 credentials for artifact upload
      source /var/lib/buildkite-agent/hooks/aws-creds
    '';
  };
  users.users.buildkite-agent.extraGroups = [
    "keys"
    "docker"
  ];

  deployment.keys = {
    aws-creds = {
      keyFile = ./. + "/../static/buildkite-hook";
      destDir = "/var/lib/buildkite-agent/hooks";
      user    = "buildkite-agent";
      permissions = "0770";
    };
    buildkite-ssh-private = {
      keyFile = ./. + "/../static/buildkite-ssh";
      user    = "buildkite-agent";
    };
    buildkite-ssh-public = {
      keyFile = ./. + "/../static/buildkite-ssh.pub";
      user    = "buildkite-agent";
    };
    buildkite-token = {
      keyFile = ./. + "/../static/buildkite-token";
      user    = "buildkite-agent";
    };
  };
}
