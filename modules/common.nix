{ config, resources, pkgs, ... }:

with (import ./../lib.nix);

{
  imports = [ ./cardano-node.nix ];

  environment.systemPackages = with pkgs;
    [ git tmux vim sysstat (pkgs.nixopsUnstable or nixops) lsof ncdu tree mosh tig
      cabal2nix stack iptables ];

  services.openssh.passwordAuthentication = true;
  services.openssh.enable = true;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = devKeys;

  environment.variables.TERM = "xterm-256color";

  services.cron.enable = true;
  services.cron.systemCronJobs = [
    "*/1 * * * *  root /run/current-system/sw/lib/sa/sadc -S DISK 2 29 /var/log/saALL"
  ];

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
    binaryCachePublicKeys = [ "hydra.serokell.io-1:qayq5mjxrNSx0QTRcpfD74pmRMHSmZCHy5vOrT8eF88=" ];
  };

  networking.firewall.enable = false;
  # Mosh
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
} // optionalAttrs (generatingAMI == false) {
  deployment.targetEnv = "ec2";
  deployment.ec2.instanceType = "t2.large";
  deployment.ec2.region = region;
  deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
  deployment.ec2.securityGroups = ["cardano-deployment"];
  deployment.ec2.ami = (import ./../modules/amis.nix).${config.deployment.ec2.region};
  deployment.ec2.accessKeyId = "cardano-deployer";
  deployment.ec2.ebsInitialRootDiskSize = 8;
}
