{ config, lib, pkgs, name, ... }:

let
  commonLib = import ../lib.nix;
  cfg = config.services.buildkite-services;
in with lib;
{
  imports = [
    ./auto-gc.nix
    ./nix_nsswitch.nix
    ./docker-builder.nix
  ];

  options = {
    services.buildkite-services = {
      metadata = mkOption {
        type = types.str;
        default = "system=x86_64-linux";
        description = ''
          Metadata associated with a buildkite agent.
        '';
        example = "system=x86_64-linux";
      };
    };
  };

  config = {
    services.buildkite-agent = {
      enable = true;
      name   = name;
      openssh.privateKeyPath = "/run/keys/buildkite-ssh-private";
      openssh.publicKeyPath  = "/run/keys/buildkite-ssh-public";
      tokenPath              = "/run/keys/buildkite-token";
      meta-data              = cfg.metadata;
      runtimePackages        = with pkgs; [
         bash gnutar gzip bzip2 xz
         git git-lfs
         nix
      ];
      hooks.environment = ''
        # Provide a minimal build environment
        export NIX_BUILD_SHELL="/run/current-system/sw/bin/bash"
        export PATH="/run/current-system/sw/bin:$PATH"

        # Provide NIX_PATH, unless it's already set by the pipeline
        if [ -z "''${NIX_PATH:-}" ]; then
            # see iohk-ops/modules/common.nix (system.extraSystemBuilderCmds)
            export NIX_PATH="nixpkgs=/run/current-system/nixpkgs"
        fi

        # load S3 credentials for artifact upload
        source /var/lib/buildkite-agent/hooks/aws-creds

        # load extra credentials for user services
        source /var/lib/buildkite-agent/hooks/buildkite-extra-creds
      '';
      hooks.pre-command = ''
        # Clean out the state that gets messed up and makes builds fail.
        rm -rf ~/.cabal
      '';
      extraConfig = ''
        git-clean-flags="-ffdqx"
      '';
    };
    users.users.buildkite-agent.extraGroups = [
      "keys"
      "docker"
    ];

    # Grant CI and dev tools people access to buildkite agents
    users.users.root.openssh.authorizedKeys.keys = commonLib.ciInfraKeys;

    deployment.keys = {
      aws-creds = {
        keyFile = ./. + "/../static/buildkite-hook";
        destDir = "/var/lib/buildkite-agent/hooks";
        user    = "buildkite-agent";
        permissions = "0770";
      };

      # Project-specific credentials to install on Buildkite agents.
      buildkite-extra-creds = {
        keyFile = ./. + "/../static/buildkite-hook-extra-creds.sh";
        destDir = "/var/lib/buildkite-agent/hooks";
        user    = "buildkite-agent";
        permissions = "0770";
      };

      # SSH keypair for buildkite-agent user
      buildkite-ssh-private = {
        keyFile = ./. + "/../static/buildkite-ssh";
        user    = "buildkite-agent";
      };
      buildkite-ssh-public = {
        keyFile = ./. + "/../static/buildkite-ssh.pub";
        user    = "buildkite-agent";
      };

      # GitHub deploy key for input-output-hk/hackage.nix
      buildkite-hackage-ssh-private = {
        keyFile = ./. + "/../static/buildkite-hackage-ssh";
        user    = "buildkite-agent";
      };

      # GitHub deploy key for input-output-hk/stackage.nix
      buildkite-stackage-ssh-private = {
        keyFile = ./. + "/../static/buildkite-stackage-ssh";
        user    = "buildkite-agent";
      };

      # GitHub deploy key for input-output-hk/haskell.nix
      # (used to update gh-pages documentation)
      buildkite-haskell-dot-nix-ssh-private = {
        keyFile = ./. + "/../static/buildkite-haskell-dot-nix-ssh";
        user    = "buildkite-agent";
      };

      # API Token for BuildKite
      buildkite-token = {
        keyFile = ./. + "/../static/buildkite_token";
        user    = "buildkite-agent";
      };
    };

    # Globally enable stack's nix integration so that stack builds have
    # the necessary dependencies available.
    environment.etc."stack/config.yaml".text = ''
      nix:
        enable: true
    '';

    systemd.services.buildkite-agent-custom = {
      wantedBy = [ "buildkite-agent.service" ];
      script = ''
        mkdir -p /build
        chown -R buildkite-agent:nogroup /build
      '';
      serviceConfig = {
        Type = "oneshot";
      };
    };
  };
}
