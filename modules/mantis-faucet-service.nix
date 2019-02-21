{ pkgs, lib, nodes, options, config, ... }:

with lib; with builtins; with import ../lib.nix;
with import <goguen/default.nix> { inherit pkgs; }; {
  options.services.mantis-faucet = {
    mantis-node = mkOption {
      type = types.str;
      description = "Mantis node this faucet uses";
      example = "mantis-a-0";
    };
  };

  imports = [
    <module/common.nix>
  ];

  config = let
    mantis-node-name = config.services.mantis-faucet.mantis-node;
    mantis-node-ip   = nodeDryRunnableIP nodes.${mantis-node-name};
    goguenPkgs       = import <goguen/default.nix> { inherit pkgs; };
  in with goguenPkgs; {
    nix.requireSignedBinaryCaches = false;

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 8099 5555 ];
      allowedUDPPorts = [ 8125 ];
      trustedInterfaces = [ "lo" ];
    };

    users.users.mantis =
        { isNormalUser = true;
          home = "/home/mantis";
          description = "Mantis user";
          extraGroups = [ "systemd-journal" ];
        };

    systemd.services.mantis = {
      wantedBy = [ "multi-user.target" ];
      enable = true;

      path = with pkgs; [ openjdk8 gawk gnused ];

      serviceConfig = {
        PermissionsStartOnly = true;
        TimeoutStartSec = "0";
        Restart = "always";
      };

      preStart = ''
        mkdir -p ${config.users.users.mantis.home}/conf
        mkdir -p ${config.users.users.mantis.home}/faucet-keystore
        cat >${config.users.users.mantis.home}/conf/faucet.conf<<'CONF'
        faucet {
          wallet-password = ""
          keystore-dir = "/home/mantis/faucet-keystore"
          keyfile = "mallet.json"
          tx-gas-price = 5000000000
          tx-gas-limit = 90000
          tx-value = 30000000000000000
          tx-data = "c9876465706f736974c0" # RLP-encoded "deposit" method call with no arguments
          listen-interface = "0.0.0.0"
          listen-port = 8099
          cors-allowed-origins = "*"
          rpc-address = "http://${mantis-node-ip}:8546/"
          min-request-interval = 1.minute
          latest-timestamp-cache-size = 1024
        }
        CONF
        cp /root/mallet.json ${config.users.users.mantis.home}/faucet-keystore/mallet.json
        chown -R $(id -u mantis):$(id -g mantis) ${config.users.users.mantis.home}/conf
        chmod 600 ${config.users.users.mantis.home}/faucet-keystore/mallet.json
      '';

      script = "${goguenPkgs.mantis} faucet -Dconfig.file=/home/mantis/conf/faucet.conf -c 'source /home/mantis/.profile && ${goguenPkgs.mantis} -Dconfig.file=/home/mantis/conf/faucet.conf faucet'";
    };
  };
}
