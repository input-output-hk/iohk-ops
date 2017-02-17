with (import ./../lib.nix);

{
  network.description = "Serokell infrastructure";

  hydra = { config, pkgs, resources, ... }: {
    # TODO: Slack integration
    # On first setup:

    # Locally: $ ssh-keygen -C "hydra@hydra.example.org" -N "" -f static/id_buildfarm
    # On Hydra: $ /run/current-system/sw/bin/hydra-create-user alice --full-name 'Alice Q. User' --email-address 'alice@example.org' --password foobar --role admin

    imports = [
      ./../modules/hydra-slave.nix
      ./../modules/hydra-master.nix
      ./../modules/common.nix
    ];

    networking.firewall.enable = mkForce true;

    deployment.ec2 = {
      # 16G memory
      instanceType = mkForce "r3.large";
      ebsInitialRootDiskSize = mkForce 200;
      elasticIPv4 = resources.elasticIPs.hydra-ip;
      associatePublicIpAddress = true;
    };
  };

  sl-explorer = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/common.nix
    ];

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.nginx = {
      enable = true;
      virtualHosts = {
       "cardano-explorer-dev.iohk.io" = {
         forceSSL = true;
         enableACME = true;
         locations."/" = {
           root = /home/production/cardano-sl-explorer-frontend/dist;
         };
       };
     };
    };

    deployment.ec2 = {
      instanceType = mkForce "t2.medium";
      ebsInitialRootDiskSize = mkForce 50;
      elasticIPv4 = resources.elasticIPs.sl-explorer-ip;
      associatePublicIpAddress = true;
    };
  };

  cardano-deployer = { config, pkgs, resources, ... }: {
    imports = [ ./../modules/common.nix ];

    users = {
      users.staging = {
        description     = "cardano staging";
        group           = "staging";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.staging = {};

      users.production = {
        description     = "cardano production";
        group           = "production";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.production = {};

    };

    networking.firewall.allowedTCPPortRanges = [
      { from = 24962; to = 25062; }
    ];
    networking.firewall.enable = mkForce true;

    deployment.ec2 = {
      # 16G memory needed for 100 nodes evaluation
      instanceType = mkForce "r3.large";
      ebsInitialRootDiskSize = mkForce 50;
      elasticIPv4 = resources.elasticIPs.cardanod-ip;
      associatePublicIpAddress = true;
      ami = mkForce "ami-01f7306e";
    };
  };
  resources = {
    inherit ec2KeyPairs;
    elasticIPs = {
      hydra-ip = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
      sl-explorer-ip = { inherit region accessKeyId; };
    };
  };
}
