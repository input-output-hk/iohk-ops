with (import ./../lib.nix);

{
  network.description = "Serokell infrastructure";

  hydra = { config, pkgs, resources, ... }: {
    imports = [
      ./../modules/hydra-slave.nix
      ./../modules/hydra-master.nix
      ./../modules/common.nix
    ];

    networking.firewall.enable = mkForce true;
 
    # https://github.com/peti/hydra-tutorial
    # https://github.com/NixOS/hydra/pull/418/files
    # TODO: Hydra, slack integration, declarative jobsets
    # TODO: https://hydra.serokell.io/jobset/rscoin/master#tabs-configuration
    # TODO: https://hydra.serokell.io/jobset/cardano/master#tabs-configuration

    deployment.ec2 = {
      # 16G memory
      instanceType = mkForce "r3.large";
      ebsInitialRootDiskSize = mkForce 200;
      elasticIPv4 = resources.elasticIPs.hydra-ip;
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
        openssh.authorizedKeys.keys = devKeys;
      };
      groups.staging = {};

      users.production = {
        description     = "cardano production";
        group           = "production";
        createHome      = true;
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
    };
  };
  resources = {
    inherit (ec2Keys.resources) ec2KeyPairs;
    elasticIPs = {
      hydra-ip = { inherit region accessKeyId; };
      cardanod-ip = { inherit region accessKeyId; };
    };
  };
}
