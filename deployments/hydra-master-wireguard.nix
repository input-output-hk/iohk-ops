let
  org = "IOHK";
  region = "eu-central-1";
in {
  hydra = { config, resources, ... }: {
    deployment = {
      ec2.securityGroups = [
        resources.ec2SecurityGroups."allow-wireguard-in-${region}-${org}"
      ];
      keys."hydra.wgprivate" = {
        destDir = "/etc/wireguard";
        keyFile = ../static/hydra.wgprivate;
      };
    };
    networking.firewall.allowedUDPPorts = [ 51820 ];
    networking.wireguard.interfaces.wg0 = {
      ips = [ "192.168.20.2/24" ];
      listenPort = 51820;
      privateKeyFile = "/etc/wireguard/hydra.wgprivate";
      peers = [
        { # Prod deployer
          # The prod deployer will utilize the public hydra endpoint
          publicKey = "K0tak4QvhkOGV2aY5G/wlhU71zwI+tNC3l+4U1Rsxo=";
          allowedIPs = [ "192.168.20.3/32" ];
          persistentKeepalive = 30;
        }
        {
          publicKey = "asG7R996ieVEhoeBHTK2DVNI664dfeBalL5dPqHIcXo=";
          allowedIPs = [ "192.168.20.0/24" ];
          endpoint = "monitoring.${config.global.dnsDomainname}:51820";
          persistentKeepalive = 30;
        }
        { # mac-mini-1
          publicKey = "eGEdIydAFSKgVEQ5RKI08uBGfZeQzVuQOrN962FB/SA=";
          allowedIPs = [ "192.168.20.30/32" ];
          persistentKeepalive = 30;
        }
      ];
    };
  };
}
