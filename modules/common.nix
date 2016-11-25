{ config, resources, pkgs, lib, ... }:

let
  secret = import ./secret.nix;
  cardano-sl = import ./../default.nix { inherit pkgs; };
  generatingAMI = builtins.getEnv "GENERATING_AMI";
in {
  imports = [ ./cardano-node.nix ];

  environment.systemPackages = with pkgs;
    [ git tmux vim sysstat nixops cardano-sl lsof ];

  services.openssh.passwordAuthentication = true;
  services.openssh.enable = true;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = secret.devKeys;

  environment.variables.TERM = "xterm-256color";

  services.cron.enable = true;
  services.cron.systemCronJobs = [
    "*/1 * * * *  root /run/current-system/sw/lib/sa/sadc -S DISK 2 29 /var/log/saALL"
  ];

  networking.firewall.enable = false;
} // lib.optionalAttrs (generatingAMI != "1") {
  deployment.targetEnv = "ec2";
  deployment.ec2.instanceType = "t2.large";
  deployment.ec2.region = "eu-central-1";
  deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
  deployment.ec2.securityGroups = [secret.securityGroup];
  deployment.ec2.ami = (import ./../modules/amis.nix).${config.deployment.ec2.region};
  deployment.ec2.accessKeyId = "cardano-deployer";
  deployment.ec2.ebsInitialRootDiskSize = 6;
}
