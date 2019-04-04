{ config, lib, pkgs, ... }:

# This is a minimal NixOS configuration for the deployer so that
# devops can login and run iohk-ops to fully deploy the deployer and
# then other hosts.

let
  username = "deployer";

in {
    imports = [
      <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ];
    ec2.hvm = true;

    users.users.${username} = {
      isNormalUser = true;
      description = "Deploy the deployer";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOdrHC+zcP4K/q246QJNTKd4PeSOydWJxVk1k7vVCOCeE310UWWljbmfRwWRvJw8snmwBl/VrB2B+jnij4Tmaqm37fHA6+9CaSu4Gy5EjiWT2s8QfhGw8ZA9iwNwx091EW+nYm02Q/fVny8ItnWHPg9W2iCxTFXJgXz0POEpKojZOP/351U8+hwO4a+4Qm/tDBpsJrizDOZyEcWvxwhr+3zk9LviQ77NPRU2UBlEQcuZeXCTENtSTav38AaOGlMx+0SBJjrvQAflG6C1Wg1b4RZSu1bQv3sLwczNYq8ilpOQDdCljYMZbfhJL98fTsAty80hYBv2Pc5sn0xZGvYNz8RgEWtCEaxiXzV/LGr5VswsQ9sQR+hLebp3aSYXhKewrVrC3J5OqRgmzaUbbP+Ies+9iTXOdi0Yl3glwEltuCTaif4BWU0krPN6yC1h41BXe1FPYyjE89CawqkPQ051Ss7iGUBqDwWN79odjAnMJa/9VQhnCmcC8tPNd7K7lR6sHGo9PPRCQ2k3Rh0VE5Pxf13307PZ7ksfCyey6lhqWhNcd1iNoo99nJdpLEt7pJOAoD/2U/rqfD69Wem9ZAHg33V5XIOqLeY6/5yaa1wnIb47Io+RArkoICZcxhDlp12/IcVIuZn6hNI67QZ09MLoQS2Q+QpPAWSTZDBViEbNwSAQ== jbgi@howard"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDDwRtXm1TviRRjstPHV6G+to0P7lhN5F4Za5fMxva9MbY3XequPBBU5/HjoyZUTcZYN7bVlh9TFLQW6GrwYtL8g6W7+qj9vjZAT+pdrnpLgN+mGXppzsIbe8SZdLj11+nrL+jr1EBDnu4CmIeGfGCeKmQdYcXHBxDOYUxl80Qqjw4SKzLCWa0NAiJPaO+O1BQ1gjjDSTGumTq/DFtYi0yCjhhgXRKLQFZeOc4eV3uUXzqqwKb8i89sUFNIxPnZgEpMC5IX33r8+9CcibhDvFXxhCbEhwyxAlygzJCdntwRzIigOHxBiZV+KW9nRy/sUUC/82zB6BHZPdYV9Gb3r2740BR5jTac9Qps7MkaGuFANDkjy4ASC9DiL3TGoWjiScF100kbHsBDnEqzsybQrDXxpgTd8PiqZq9I1l1as2UoeuR3IPHO7zBbgbCy4rv9a7ZeITsPT7HcRDGHsVT762KnxVxQnR3m0CpoKGKWOKngMVRCTYsQ7Ng7f/ade9isduccrMeeTjGdkeC8QGS4VnfIEEqfHPJBS8/nree40vpvtWsvKHM346GQRm6A2UI14yBZIr/SoLQEZZP3TGwcOAA4Ze3BNGjPT38gnrPO3M8HiUJCyK3RS8GMOVr2K35aS+YTKOkLRYt4vM+vwSIWLtNgjq5kXh3HHOwFAWFn2m+ZBw== koserge-2017-04"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbtjKqNiaSVMBBSK4m97LXCBwhHMfJh9NBGBxDg+XYCLOHuuKw2MsHa4eMtRhnItELxhCAXZg0rdwZTlxLb6tzsVPXAVMrVniqfbG2qZpDPNElGdjkT0J5N1X1mOzmKymucJ1uHRDxTpalpg5d5wyGZgwVuXerep3nnv2xIIYcHWm5Hy/eG0pRw6XuQUeMc3xSU5ChqZPwWqhdILkYteKMgD8vxkfmYE9N/2fPgKRmujKQ5SA0HoJYKBXo29zGQY5r6nt3CzuIANFD3vG3323Znvt8dSRd1DiaZqKEDWSI43aZ9PX2whYEDyj+L/FvQa78Wn7pc8Nv2JOb7ur9io9t michael.bishop"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC9soUucXq/jZvm4u5a49m+aB2+l1w8VRyrjcjMGHZslHZYtMuI6vNJ9AsK+cFEirq888ITa/ucriMInukvzP3WCRthvWgPINsbupOpaHxX0k6N2RRYZQbSeKMzjhnoIkM1GkrjHuRAEjUN4RbcbEzhgVGranb8+Mb6UIkFfCwgJJdzX8X9QWStVoUsO7C+x8+m1cYkxdWYrpGqyXZ+g9P7K2rKlfoz4kEAyo4Mivh8+xmO7bPSLpGuBgM7bt4Yyaq1YSuLOp5f5P4Nsa5MmXKANumEZqVNzgLlommB/3xr7N6q+K1nLt/OxvrxrNVMpwL/TYmTRGQ/UVQziglCQz1p rodney@aurora"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5PPVdfBhmWxWYjSosuyjMdIYNjYC/ekz+Whv27wrFNHqxeGgKbXslUTwZX0r+zu/nlJnX9nj3zdVV9LosBB8JF9tfJGui9aBfPuxoIq9SMFSdcpZ8aOh4ZITv7zbsRHMECE8q7D5/a+7UZyTy8pv9g5SuCerHh3m//NIbo08OS9rt8SjqVio+B+rseLF960U3U3wTCtOA+VauTuE4kZfSfmQlEYUjaN3qwp4s5jpO7pgnGxshuqayRyuwJfRa/RYWB5ouSjyxTuo33K42EqT4XFoURkj7evJB5SRR7pm4vJCx4VkclIVmpLIcBiyWje+60zyKhAZEQVqKXedkuQ9748wZl07C6Czs4QiloGAjXv/tRm9YSdoeG5JhskEA8z2SCEQARJGquPH+f5vBltHeVC5K5LW94gSP9bfVBitcCgONVxUguCu0PmJUYKcVVjRi3KtJJzDSTDCjjN3e/mszrZY921yvVEkb7mFATBiHeSdrt55gKcG1vfTToLALIJJFQpGCwAMYUjKEcgq4PZa1UdCY/ynvynLds3mge4Y/X3EnLFsJaepfgNyPnnPg67kEda8uRSDYT8LaoqJpDzc7RQeY4BOfJfAxa8qMDHmp4W+dxHqrMphbH66fwUJAx1MWV8AoPFW0TGrDb3AnHBgoRt/5Fnz2ymy92Wb1KAIt3w== sam@optina"
      ];
    };

    users.extraUsers.root.openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOdrHC+zcP4K/q246QJNTKd4PeSOydWJxVk1k7vVCOCeE310UWWljbmfRwWRvJw8snmwBl/VrB2B+jnij4Tmaqm37fHA6+9CaSu4Gy5EjiWT2s8QfhGw8ZA9iwNwx091EW+nYm02Q/fVny8ItnWHPg9W2iCxTFXJgXz0POEpKojZOP/351U8+hwO4a+4Qm/tDBpsJrizDOZyEcWvxwhr+3zk9LviQ77NPRU2UBlEQcuZeXCTENtSTav38AaOGlMx+0SBJjrvQAflG6C1Wg1b4RZSu1bQv3sLwczNYq8ilpOQDdCljYMZbfhJL98fTsAty80hYBv2Pc5sn0xZGvYNz8RgEWtCEaxiXzV/LGr5VswsQ9sQR+hLebp3aSYXhKewrVrC3J5OqRgmzaUbbP+Ies+9iTXOdi0Yl3glwEltuCTaif4BWU0krPN6yC1h41BXe1FPYyjE89CawqkPQ051Ss7iGUBqDwWN79odjAnMJa/9VQhnCmcC8tPNd7K7lR6sHGo9PPRCQ2k3Rh0VE5Pxf13307PZ7ksfCyey6lhqWhNcd1iNoo99nJdpLEt7pJOAoD/2U/rqfD69Wem9ZAHg33V5XIOqLeY6/5yaa1wnIb47Io+RArkoICZcxhDlp12/IcVIuZn6hNI67QZ09MLoQS2Q+QpPAWSTZDBViEbNwSAQ== jbgi@howard"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDDwRtXm1TviRRjstPHV6G+to0P7lhN5F4Za5fMxva9MbY3XequPBBU5/HjoyZUTcZYN7bVlh9TFLQW6GrwYtL8g6W7+qj9vjZAT+pdrnpLgN+mGXppzsIbe8SZdLj11+nrL+jr1EBDnu4CmIeGfGCeKmQdYcXHBxDOYUxl80Qqjw4SKzLCWa0NAiJPaO+O1BQ1gjjDSTGumTq/DFtYi0yCjhhgXRKLQFZeOc4eV3uUXzqqwKb8i89sUFNIxPnZgEpMC5IX33r8+9CcibhDvFXxhCbEhwyxAlygzJCdntwRzIigOHxBiZV+KW9nRy/sUUC/82zB6BHZPdYV9Gb3r2740BR5jTac9Qps7MkaGuFANDkjy4ASC9DiL3TGoWjiScF100kbHsBDnEqzsybQrDXxpgTd8PiqZq9I1l1as2UoeuR3IPHO7zBbgbCy4rv9a7ZeITsPT7HcRDGHsVT762KnxVxQnR3m0CpoKGKWOKngMVRCTYsQ7Ng7f/ade9isduccrMeeTjGdkeC8QGS4VnfIEEqfHPJBS8/nree40vpvtWsvKHM346GQRm6A2UI14yBZIr/SoLQEZZP3TGwcOAA4Ze3BNGjPT38gnrPO3M8HiUJCyK3RS8GMOVr2K35aS+YTKOkLRYt4vM+vwSIWLtNgjq5kXh3HHOwFAWFn2m+ZBw== koserge-2017-04"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbtjKqNiaSVMBBSK4m97LXCBwhHMfJh9NBGBxDg+XYCLOHuuKw2MsHa4eMtRhnItELxhCAXZg0rdwZTlxLb6tzsVPXAVMrVniqfbG2qZpDPNElGdjkT0J5N1X1mOzmKymucJ1uHRDxTpalpg5d5wyGZgwVuXerep3nnv2xIIYcHWm5Hy/eG0pRw6XuQUeMc3xSU5ChqZPwWqhdILkYteKMgD8vxkfmYE9N/2fPgKRmujKQ5SA0HoJYKBXo29zGQY5r6nt3CzuIANFD3vG3323Znvt8dSRd1DiaZqKEDWSI43aZ9PX2whYEDyj+L/FvQa78Wn7pc8Nv2JOb7ur9io9t michael.bishop"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC9soUucXq/jZvm4u5a49m+aB2+l1w8VRyrjcjMGHZslHZYtMuI6vNJ9AsK+cFEirq888ITa/ucriMInukvzP3WCRthvWgPINsbupOpaHxX0k6N2RRYZQbSeKMzjhnoIkM1GkrjHuRAEjUN4RbcbEzhgVGranb8+Mb6UIkFfCwgJJdzX8X9QWStVoUsO7C+x8+m1cYkxdWYrpGqyXZ+g9P7K2rKlfoz4kEAyo4Mivh8+xmO7bPSLpGuBgM7bt4Yyaq1YSuLOp5f5P4Nsa5MmXKANumEZqVNzgLlommB/3xr7N6q+K1nLt/OxvrxrNVMpwL/TYmTRGQ/UVQziglCQz1p rodney@aurora"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5PPVdfBhmWxWYjSosuyjMdIYNjYC/ekz+Whv27wrFNHqxeGgKbXslUTwZX0r+zu/nlJnX9nj3zdVV9LosBB8JF9tfJGui9aBfPuxoIq9SMFSdcpZ8aOh4ZITv7zbsRHMECE8q7D5/a+7UZyTy8pv9g5SuCerHh3m//NIbo08OS9rt8SjqVio+B+rseLF960U3U3wTCtOA+VauTuE4kZfSfmQlEYUjaN3qwp4s5jpO7pgnGxshuqayRyuwJfRa/RYWB5ouSjyxTuo33K42EqT4XFoURkj7evJB5SRR7pm4vJCx4VkclIVmpLIcBiyWje+60zyKhAZEQVqKXedkuQ9748wZl07C6Czs4QiloGAjXv/tRm9YSdoeG5JhskEA8z2SCEQARJGquPH+f5vBltHeVC5K5LW94gSP9bfVBitcCgONVxUguCu0PmJUYKcVVjRi3KtJJzDSTDCjjN3e/mszrZY921yvVEkb7mFATBiHeSdrt55gKcG1vfTToLALIJJFQpGCwAMYUjKEcgq4PZa1UdCY/ynvynLds3mge4Y/X3EnLFsJaepfgNyPnnPg67kEda8uRSDYT8LaoqJpDzc7RQeY4BOfJfAxa8qMDHmp4W+dxHqrMphbH66fwUJAx1MWV8AoPFW0TGrDb3AnHBgoRt/5Fnz2ymy92Wb1KAIt3w== sam@optina"
    ];

    security.sudo.enable = true;
    security.sudo.wheelNeedsPassword = false;

    networking.hostName = "deployer";

    nix = {
      nixPath = [ "nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz"
                ];
      extraOptions = ''
        build-cores = 8
        auto-optimise-store = true
      '';
      binaryCaches = [
        "https://cache.nixos.org"
        "https://hydra.iohk.io"
      ];
      binaryCachePublicKeys = [
       "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
       "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
    };

    environment.systemPackages =
      with pkgs; [ tmux git vim nixops ];
}
