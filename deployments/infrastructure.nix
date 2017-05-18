with (import ./../lib.nix);

{
  hydra = { config, pkgs, ... }: {
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
  };

  cardano-deployer = { config, pkgs, ... }: {
    imports = [
      ./../modules/common.nix
    ];

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
        openssh.authorizedKeys.keys = [];  # this account is obsolete
      };
      groups.production = {};

      users.live-production = {
        description     = "cardano live-production";
        group           = "live-production";
        createHome      = true;
        isNormalUser = true;
        openssh.authorizedKeys.keys = [domenKey georgeeeKey kosergeKey jakeKey];
      };
      groups.live-production = {};
    };

    networking.firewall.allowedTCPPortRanges = [
      { from = 24962; to = 25062; }
    ];
    networking.firewall.enable = mkForce true;
  };
}
