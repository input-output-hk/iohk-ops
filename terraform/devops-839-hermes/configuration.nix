{ config, pkgs, ... }:
{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
  ];
  ec2.hvm = true;

  users.users.vincent =
    { isNormalUser = true;
      description = "Vincent Hanquez";
      extraGroups = [ "users" "wheel" ];
      openssh.authorizedKeys.keys = [ "${ssh_key}" ];
    };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs;
    [ tmux htop psmisc which lsof strace ncdu
      wget vim git ];

  services.fail2ban.enable = true;

  networking.hostName = "${hostname}";

  fileSystems = {
    "/data" = {
      device = "/dev/xvdf";
      fsType = "ext4";
      label = "data";
      autoFormat = true;
      autoResize = true;
    };
  };
}
