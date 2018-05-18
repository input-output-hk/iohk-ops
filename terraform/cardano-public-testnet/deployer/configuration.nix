{ config, lib, pkgs, ... }:

let
  env = "${env}";
  username = "${username}";

  src = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-ops";
    rev = "9579e210d446821ce30658b5b9db9e6590e1aedf";
    sha256 = "0filamx40g9yhkvacvs0fwc5f1xkdfj6fga98qb1sqd0szdv3x8q";
  };
  iohk-ops = import src {};
  localLib = import (src + "/lib.nix");

in {
    imports = [
      <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ];
    ec2.hvm = true;

    users.users.${username} =
      { isNormalUser = true;
        description = "Deploy the deployer";
        extraGroups = [ "users" "wheel" ];
        openssh.authorizedKeys.keys = localLib.devOpsKeys;
      };

    security.sudo.enable = true;
    security.sudo.wheelNeedsPassword = false;

    networking.hostName = "${env}-deployer";

    nix = {
      binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
      binaryCaches = [ "https://hydra.iohk.io" ];
    };

    environment.systemPackages = with pkgs;
      [ tmux git vim iohk-ops.iohk-ops ];

    services.fail2ban.enable = true;
}
