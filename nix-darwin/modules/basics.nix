{ config, lib, pkgs, ... }:

let
  opsLib = import ../../lib.nix;
  nix-darwin = (import ../test.nix { host = null; port = null; hostname = null; }).nix-darwin;
in {
  imports = [ ./double-builder-gc.nix ./caffeinate.nix ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    nix
    tmux
    ncdu
    git
  ] ++ (if pkgs.stdenv.isDarwin then [
    darwin.cctools
  ] else []);

  # Set all macs to same timezone
  time.timeZone = "GMT";

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 2;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 4;
  nix.buildCores = 0;
  nix.useSandbox = false;  # this seems to break things when enabled
  nix.extraOptions = ''
    gc-keep-derivations = true
    gc-keep-outputs = true

    # Max of 8 hours for building any given derivation on macOS.
    # The long timeout should give enough time to build a cross GHC.
    # See ../modules/hydra-slave.nix for Linux setting
    timeout = ${toString (3600 * 8)}

    # Quickly kill stuck builds
    max-silent-time = ${toString (60 * 15)}

    sandbox = false
    extra-sandbox-paths = /System/Library/Frameworks /usr/lib /System/Library/PrivateFrameworks
  '';

  nix.binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ];
  nix.binaryCaches = [ "http://192.168.3.1:8081" ];
  nix.trustedUsers = [ "@admin" ];

  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs"
    "darwin-config=/Users/nixos/.nixpkgs/darwin-configuration.nix"
    "darwin=${nix-darwin}"
  ];

  ########################################################################

  environment.etc."per-user/root/ssh/authorized_keys".text
    = lib.concatStringsSep "\n" opsLib.ciInfraKeys + "\n";

  environment.etc."per-user/nixos/ssh/authorized_keys".text
    = lib.concatStringsSep "\n" opsLib.ciInfraKeys + "\n";

  ########################################################################

  services.nix-daemon.enable = true;

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  system.activationScripts.postActivation.text = ''
    printf "disabling spotlight indexing... "
    mdutil -i off -d / &> /dev/null
    mdutil -E / &> /dev/null
    echo "ok"
    printf "disabling screensaver..."
    defaults write /Library/Preferences/com.apple.screensaver loginWindowIdleTime 0
    echo "ok"

    for user in admin nixos buildkite builder; do
        authorized_keys=/etc/per-user/$user/ssh/authorized_keys
        user_home=/Users/$user
        printf "configuring ssh keys for $user... "
        if [ -f $authorized_keys ]; then
            mkdir -p $user_home/.ssh
            cp -f $authorized_keys $user_home/.ssh/authorized_keys
            chown $user: $user_home $user_home/.ssh $user_home/.ssh/authorized_keys
            echo "ok"
        else
            echo "nothing to do"
        fi
    done
  '';

  ########################################################################
}
