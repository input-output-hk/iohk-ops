{ config, lib, pkgs, ... }:

# This is a minimal NixOS configuration for the deployer so that
# devops can login and run iohk-ops to fully deploy the deployer and
# then other hosts.

let
  # templated by terraform
  env = "${env}";
  username = "${username}";

  src = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-ops";
    rev = "fe0ad900c64e2f769a7ff6a4c86b809cb24ccd93";
    sha256 = "1c84r3mxa44ywpay94rrbxzc7q08xvhp44d7z5hkkzkmaf429cgh";
  };
  iohk-pkgs = import src {};
  localLib = import (src + "/lib.nix");

in {
    imports = [
      <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ];
    ec2.hvm = true;

    users.users.${username} = {
      isNormalUser = true;
      description = "Deploy the deployer";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = localLib.devOpsKeys;
    };

    users.extraUsers.root.openssh.authorizedKeys.keys = localLib.devOpsKeys;

    security.sudo.enable = true;
    security.sudo.wheelNeedsPassword = false;

    networking.hostName = "${env}-deployer";

    nix = {
      binaryCaches = [
        "https://cache.nixos.org"
        "https://hydra.iohk.io"
      ];
      binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    };

    environment.systemPackages =
      with pkgs; [ tmux git vim ] ++
      with iohk-pkgs; [ nixops iohk-ops ];
}
