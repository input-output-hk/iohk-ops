{ config, pkgs, ... }:

with (import ./../lib.nix);

{
  imports = [
    ./cardano-node.nix
  ];

  environment.systemPackages = with pkgs;
    [ git tmux vim sysstat nixops lsof ncdu tree mosh tig
      cabal2nix stack iptables ];

  services.openssh.passwordAuthentication = true;
  services.openssh.enable = true;

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

    # use our hydra builds
    trustedBinaryCaches = [ "https://cache.nixos.org" "https://hydra.iohk.io" ];
    binaryCaches = trustedBinaryCaches;
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  networking.firewall.enable = false;
  # Mosh
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
}
