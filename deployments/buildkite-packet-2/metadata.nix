with (import ../../lib.nix);
{ 
      networking.hostName = "buildkite-packet-2";
      networking.dhcpcd.enable = false;
      networking.defaultGateway = {
        address =  "147.75.33.106";
        interface = "bond0";
      };
      networking.defaultGateway6 = {
        address = "2604:1380:2000:e400::c";
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
              via = "10.80.125.12";
            }
          ];
          addresses = [
            {
              address = "147.75.33.107";
              prefixLength = 31;
            }
            {
              address = "10.80.125.13";
              prefixLength = 31;
            }
          ];
        };

        ipv6 = {
          addresses = [
            {
              address = "2604:1380:2000:e400::d";
              prefixLength = 127;
            }
          ];
        };
      };
    
      users.users.root.openssh.authorizedKeys.keys = devOpsKeys ++ [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI+ej2JpsbxyCtScmGZWseA+TeHica1a1hGtTgrX/mi cardano@ip-172-31-26-83.eu-central-1.compute.internal"
      ];
    
      users.users.root.initialHashedPassword = "$6$R3UUvu9jnl4D137n$VzTkNbxZYdSEPLiz2ulI3hnbpf0DscWBLLQ0cOu4tuywHF4ibR0.QDtURC7YX3S2Cq3xAJOv54PpqYzqMjLGD0";
     }
