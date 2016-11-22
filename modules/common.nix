{ resources, pkgs, ... }:

let
  secret = import ./secret.nix;
  cardano-sl = import ./../default.nix { inherit pkgs };
  generatingAMI = builtins.getEnv "GENERATING_AMI";
in {
  imports = [ ./modules/cardano-node.nix ];

  environment.systemPackages = with pkgs;
    [ git tmux vim sysstat nixops srk-nixpkgs.cardano-sl lsof ];

  services.openssh.passwordAuthentication = true;
  services.openssh.enable = true;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = secret.devKeys;
  users.users.statReader = {
    isNormalUser = true;
    password = secret.rootPassword;
  };

  environment.variables.TERM = "xterm-256color";

  services.cron.enable = true;
  services.cron.systemCronJobs = [
    "*/1 * * * *  root /run/current-system/sw/lib/sa/sadc -S DISK 2 29 /var/log/saALL"
  ];

  networking.firewall.enable = false;
} // lib.optionalAttrs (generatingAMI != "1") {
  deployment.targetEnv = "ec2";
  deployment.ec2.accessKeyId = secret.accessKeyId;
  deployment.ec2.instanceType = "t2.large";
  deployment.ec2.securityGroups = [secret.securityGroup];
  deployment.ec2.ebsBoot = true;
  deployment.ec2.ebsInitialRootDiskSize = 6;
}
