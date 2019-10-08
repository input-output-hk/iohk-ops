{ config, lib, pkgs, name, ... }:

let
  commonLib = import ../lib.nix;
  cfg = config.services.buildkite-containers;

in with lib;
{
  imports = [
    # GC only from the host to avoid duplicating GC in containers
    ./auto-gc.nix
    # Docker module required in both the host and guest containers
    ./docker-builder.nix
  ];

  options = {
    services.buildkite-containers = {
      hostIdSuffix = mkOption {
        type = types.str;
        default = "1";
        description = ''
          A host identifier suffix which, is typically a CI server number, is used
          as part of the container name when the default container list is deployed.
          Container names are limited to 7 characters, so the default naming convention
          is ci${hostIdSuffix}-<containerNum>.  An example container name, using a
          hostIdSuffix of 2 for example, may then be ci2-4, indicating a 4th CI
          container on a 2nd host CI server.
        '';
        example = "1";
      };

      containerList = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = ''
          This parameter allows container customization on a per server basis.  If
          left as a default empty list, a predefined and fixed deployment of
          containers will pushed to each host which includes this module.  If this
          option is provided a list of attributes, each representing a Buildkite
          container, this list of containers will be used instead of the predefined
          list.  See the buildkite-agent-container.nix module for possible customizations.
          Note that container names cannot be more than 7 characters.
        '';
        example = ''
          [ { containerName = "ci1-1"; ipo4 = "10"; metadata = "system=x86_64-linux,queue=custom"; }
            { containerName = "ci1-2"; ipo4 = "11"; metadata = "system=x86_64-linux,queue=custom"; } ];
        '';
      };
    };
  };

  config = let
    createBuildkiteContainer = { containerName                                 # The desired container name
                               , baseVethIp ? "10.254.1"                       # The first 3 octets of the IPv4 host and guest virtual eth nic IP
                               , hostIpo4 ? "1"                                # The fourth octet of the IPv4 host virtual eth nic IP
                               , ipo4 ? "10"                                   # The fourth octet of the IPv4 container guest virtual eth nic IP
                               , metadata ? "system=x86_64-linux,queue=test"   # Agent metadata customization
                               , prio ? null                                   # Agent priority
                              }: {
      name = containerName;
      value = {
        autoStart = true;
        bindMounts = {
          "/run/keys" = {
            hostPath = "/run/keys";
          };
          "/var/lib/buildkite-agent/hooks" = {
            hostPath = "/var/lib/buildkite-agent/hooks";
          };
          "/cache" = {
            hostPath = "/cache";
            isReadOnly = false;
          };
        };
        privateNetwork = true;
        hostAddress = baseVethIp + "." + hostIpo4;
        localAddress = baseVethIp + "." + ipo4;
        config = {
          imports = [
            ./nix_nsswitch.nix
            # Docker module required in both the host and guest containers
            ./docker-builder.nix
            ./common.nix
          ];
          services.monitoring-exporters.enable = false;
          services.ntp.enable = mkForce false;
          services.buildkite-agent = {
            enable = true;
            name   = name + "-" + containerName;
            openssh.privateKeyPath = "/run/keys/buildkite-ssh-private";
            openssh.publicKeyPath  = "/run/keys/buildkite-ssh-public";
            tokenPath              = "/run/keys/buildkite-token";
            meta-data              = metadata;
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
            hooks.pre-exit = ''
              # Clean up the scratch and tmp directories
              rm -rf /scratch/* /tmp/*
            '';
            extraConfig = ''
              git-clean-flags="-ffdqx"
              ${if prio != null then "priority=${prio}" else ""}
            '';
          };
          users.users.buildkite-agent = {
            # To ensure buildkite-agent user sharing of keys in guests
            uid = 10000;
            extraGroups = [
              "keys"
              "docker"
            ];
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
              mkdir -p /build /scratch
              chown -R buildkite-agent:nogroup /build /scratch
            '';
            serviceConfig = {
              Type = "oneshot";
            };
          };
        };
      };
    };
  in {
    users.users.root.openssh.authorizedKeys.keys = commonLib.ciInfraKeys;

    # To go on the host -- and get shared to the container(s)
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

    system.activationScripts.cacheDir = {
      text = ''
        mkdir -p /cache
        chown -R buildkite-agent:nogroup /cache || true
      '';
      deps = [];
    };

    users.users.buildkite-agent = {
      home = "/var/lib/buildkite-agent";
      createHome = true;
      # To ensure buildkite-agent user sharing of keys in guests
      uid = 10000;
    };
    environment.systemPackages = [ pkgs.nixos-container ];
    networking.nat.enable = true;
    networking.nat.internalInterfaces = [ "ve-+" ];
    networking.nat.externalInterface = "bond0";

    services.fstrim.enable = true;
    services.fstrim.interval = "daily";

    containers = let
      buildkiteContainerList = if (length cfg.containerList == 0) then ([
        { containerName = "ci${cfg.hostIdSuffix}-1"; ipo4 = "10"; prio = "9"; }
        { containerName = "ci${cfg.hostIdSuffix}-2"; ipo4 = "11"; prio = "8"; }
        { containerName = "ci${cfg.hostIdSuffix}-3"; ipo4 = "12"; prio = "7"; }
        { containerName = "ci${cfg.hostIdSuffix}-4"; ipo4 = "13"; prio = "6"; }
      ]) else cfg.containerList;
    in
      builtins.listToAttrs (map createBuildkiteContainer buildkiteContainerList);
  };
}
