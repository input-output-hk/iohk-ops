{ config, pkgs, ... }:
{
    imports = [
      <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ];
    ec2.hvm = true;

    users.users.infra =
        { isNormalUser = true;
          description = "Deploy the deployer";
          extraGroups = [];
          openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC9soUucXq/jZvm4u5a49m+aB2+l1w8VRyrjcjMGHZslHZYtMuI6vNJ9AsK+cFEirq888ITa/ucriMInukvzP3WCRthvWgPINsbupOpaHxX0k6N2RRYZQbSeKMzjhnoIkM1GkrjHuRAEjUN4RbcbEzhgVGranb8+Mb6UIkFfCwgJJdzX8X9QWStVoUsO7C+x8+m1cYkxdWYrpGqyXZ+g9P7K2rKlfoz4kEAyo4Mivh8+xmO7bPSLpGuBgM7bt4Yyaq1YSuLOp5f5P4Nsa5MmXKANumEZqVNzgLlommB/3xr7N6q+K1nLt/OxvrxrNVMpwL/TYmTRGQ/UVQziglCQz1p rodney@aurora" ];
        };

    environment.systemPackages = with pkgs;
                    [ nixops ];

    services.fail2ban.enable = true;
}
