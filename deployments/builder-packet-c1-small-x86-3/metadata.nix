{ 
      networking.hostName = "builder-packet-c1-small-x86-3";
      networking.dhcpcd.enable = false;
      networking.defaultGateway = {
        address =  "147.75.80.50";
        interface = "bond0";
      };
      networking.defaultGateway6 = {
        address = "2604:1380:2000:e400::4";
        interface = "bond0";
      };
      networking.nameservers = [
        "147.75.207.207"
        "147.75.207.208"
      ];
    
      networking.bonds.bond0 = {
        driverOptions = {
          mode = "802.3ad";
          xmit_hash_policy = "layer3+4";
          lacp_rate = "fast";
          downdelay = "200";
          miimon = "100";
          updelay = "200";
        };

        interfaces = [
          "enp1s0f0" "enp1s0f1"
        ];
      };
    
      networking.interfaces.bond0 = {
        useDHCP = false;

        ipv4 = {
          routes = [
            {
              address = "10.0.0.0";
              prefixLength = 8;
              via = "10.80.125.4";
            }
          ];
          addresses = [
            {
              address = "147.75.80.51";
              prefixLength = 31;
            }
            {
              address = "10.80.125.5";
              prefixLength = 31;
            }
          ];
        };

        ipv6 = {
          addresses = [
            {
              address = "2604:1380:2000:e400::5";
              prefixLength = 127;
            }
          ];
        };
      };
    
      users.users.root.openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbtjKqNiaSVMBBSK4m97LXCBwhHMfJh9NBGBxDg+XYCLOHuuKw2MsHa4eMtRhnItELxhCAXZg0rdwZTlxLb6tzsVPXAVMrVniqfbG2qZpDPNElGdjkT0J5N1X1mOzmKymucJ1uHRDxTpalpg5d5wyGZgwVuXerep3nnv2xIIYcHWm5Hy/eG0pRw6XuQUeMc3xSU5ChqZPwWqhdILkYteKMgD8vxkfmYE9N/2fPgKRmujKQ5SA0HoJYKBXo29zGQY5r6nt3CzuIANFD3vG3323Znvt8dSRd1DiaZqKEDWSI43aZ9PX2whYEDyj+L/FvQa78Wn7pc8Nv2JOb7ur9io9t clever@amd-nixos
"
    

        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDDwRtXm1TviRRjstPHV6G+to0P7lhN5F4Za5fMxva9MbY3XequPBBU5/HjoyZUTcZYN7bVlh9TFLQW6GrwYtL8g6W7+qj9vjZAT+pdrnpLgN+mGXppzsIbe8SZdLj11+nrL+jr1EBDnu4CmIeGfGCeKmQdYcXHBxDOYUxl80Qqjw4SKzLCWa0NAiJPaO+O1BQ1gjjDSTGumTq/DFtYi0yCjhhgXRKLQFZeOc4eV3uUXzqqwKb8i89sUFNIxPnZgEpMC5IX33r8+9CcibhDvFXxhCbEhwyxAlygzJCdntwRzIigOHxBiZV+KW9nRy/sUUC/82zB6BHZPdYV9Gb3r2740BR5jTac9Qps7MkaGuFANDkjy4ASC9DiL3TGoWjiScF100kbHsBDnEqzsybQrDXxpgTd8PiqZq9I1l1as2UoeuR3IPHO7zBbgbCy4rv9a7ZeITsPT7HcRDGHsVT762KnxVxQnR3m0CpoKGKWOKngMVRCTYsQ7Ng7f/ade9isduccrMeeTjGdkeC8QGS4VnfIEEqfHPJBS8/nree40vpvtWsvKHM346GQRm6A2UI14yBZIr/SoLQEZZP3TGwcOAA4Ze3BNGjPT38gnrPO3M8HiUJCyK3RS8GMOVr2K35aS+YTKOkLRYt4vM+vwSIWLtNgjq5kXh3HHOwFAWFn2m+ZBw=="
    

        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI+ej2JpsbxyCtScmGZWseA+TeHica1a1hGtTgrX/mi cardano@ip-172-31-26-83.eu-central-1.compute.internal"
    

        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI+ej2JpsbxyCtScmGZWseA+TeHica1a1hGtTgrX/mi cardano@ip-172-31-26-83.eu-central-1.compute.internal"
    
      ];
    
      users.users.root.initialHashedPassword = "$6$RVdqpUZmSrIa7XJb$gaGwIIF8b3egFCncEua4RTvVypw05aA1R69J2ZXA7/Avx8rLc4r9YlaBy1T8MlM7ng5HASZICOhlNnUnBTp.H.";
     }
