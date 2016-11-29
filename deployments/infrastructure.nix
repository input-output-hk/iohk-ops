with (import ./../lib.nix);

{
  network.description = "Serokell infrastructure";

  hydra = { config, pkgs, lib, ... }: {
    imports = [
      ./../modules/hydra-slave.nix
      ./../modules/hydra-master.nix
      ./../modules/common.nix
    ];

    networking.firewall.enable = lib.mkForce true;
 
    # https://github.com/peti/hydra-tutorial
    # https://github.com/NixOS/hydra/pull/418/files
    # TODO: Hydra, slack integration, declarative jobsets
    # TODO: https://hydra.serokell.io/jobset/rscoin/master#tabs-configuration
    # TODO: https://hydra.serokell.io/jobset/cardano/master#tabs-configuration

    # 16G memory
    deployment.ec2.instanceType = lib.mkForce "r3.large";
    deployment.ec2.ebsInitialRootDiskSize = lib.mkForce 200;
  };

  cardano-deployer = { config, pkgs, lib, ... }: {
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
    networking.firewall.enable = lib.mkForce true;

    # 16G memory needed for 100 nodes evaluation
    deployment.ec2.instanceType = lib.mkForce "r3.large";
    deployment.ec2.ebsInitialRootDiskSize = lib.mkForce 50;
  };
} // (import ./../lib.nix).ec2Keys
