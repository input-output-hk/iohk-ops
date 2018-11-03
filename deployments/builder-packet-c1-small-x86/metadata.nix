with (import ../../lib.nix);
{ 
      networking.hostName = "builder-packet-c1-small-x86";
      networking.dhcpcd.enable = false;
      networking.defaultGateway = {
        address =  "147.75.32.120";
        interface = "bond0";
      };
      networking.defaultGateway6 = {
        address = "2604:1380:2000:e400::";
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
              via = "10.80.125.0";
            }
          ];
          addresses = [
            {
              address = "147.75.32.121";
              prefixLength = 31;
            }
            {
              address = "10.80.125.1";
              prefixLength = 31;
            }
          ];
        };

        ipv6 = {
          addresses = [
            {
              address = "2604:1380:2000:e400::1";
              prefixLength = 127;
            }
          ];
        };
      };
    
      users.users.root.openssh.authorizedKeys.keys = devOpsKeys ++ [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI+ej2JpsbxyCtScmGZWseA+TeHica1a1hGtTgrX/mi cardano@ip-172-31-26-83.eu-central-1.compute.internal"
      ];
    
      users.users.root.initialHashedPassword = "$6$gYyxlBo2/Oz4IWj4$OrfpS94k6tWRnKllaaaHClJds6lkhpFUgFqYt3xFL18kfhiu3Tz/B2eJdH.1mTt9cnT4oKfhgCYF3I/OAe5kX1";
     }
