{ config, pkgs, ... }:

with (import ./../lib.nix);

{
  imports = [
    ./cardano-node.nix
  ];

  environment.systemPackages = with pkgs;
    # nixopsUnstable: wait for 1.5.1 release
    [ git tmux vim sysstat nixopsUnstable lsof ncdu tree mosh tig
      cabal2nix stack iptables graphviz ];

  services.openssh.passwordAuthentication = true;
  services.openssh.enable = true;

  services.ntp.enable = true;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = devKeys;

  environment.variables.TERM = "xterm-256color";

  services.cron.enable = true;
  #services.cron.systemCronJobs = [
  #  "*/1 * * * *  root /run/current-system/sw/lib/sa/sadc -S DISK 2 29 /var/log/saALL"
  #];

  nix = rec {
    # use nix sandboxing for greater determinism
    useSandbox = true;

    # make sure we have enough build users
    nrBuildUsers = 30;

    # if our hydra is down, don't wait forever
    extraOptions = ''
      connect-timeout = 10
    '';

    buildCores = 0;

    nixPath = ["nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz"];

    # use our hydra builds
    trustedBinaryCaches = [ "https://cache.nixos.org" "https://hydra.iohk.io" ];
    binaryCaches = trustedBinaryCaches;
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  # Mosh
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
}
