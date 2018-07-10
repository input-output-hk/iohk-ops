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
    runtimePackages        = with pkgs; [ gnutar gzip bzip2 xz ];
    extraConfig = ''
      git-clean-flags="-ffdqx"
    '';
    hooks.pre-command = ''
      # Clean out the state that gets messed up and makes builds fail.
      rm -rf ~/.cabal

      # load AWS credentials for artifact upload
      source /var/lib/buildkite-agent/hooks/s3-creds
    '';
  };
  users.users.buildkite-agent.extraGroups = [
    "keys"
    "docker"
  ];

  deployment.keys = {
    s3-creds = {
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
