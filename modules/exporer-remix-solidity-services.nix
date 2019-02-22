{ pkgs, lib, nodes, options, config, ... }:

with lib; with builtins; with import ../lib.nix;
with import <goguen/default.nix> { inherit pkgs; }; {
  options.services.mantis-explorer = {
    mantis-node = mkOption {
      type = types.str;
      description = "Mantis node this explorer follows";
      example = "mantis-a-0";
    };
  };

  imports = [
    <module/common.nix>
  ];

  config = let
    mantis-node-name = config.services.mantis-explorer.mantis-node;
    mantis-node-ip   = nodeDryRunnablePrivateIP nodes.${mantis-node-name};
  in {
    nix.requireSignedBinaryCaches = false;

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 80 8080 5601 ];
    };

    systemd.services.solidity-service = {
      wantedBy = [ "nginx.service" ];
      before = [ "nginx.service" ];
      enable = true;

      serviceConfig = {
        TimeoutStartSec = "0";
        Restart = "always";
        User = "eth";
      };

      script = ''
        ${solidityService}/bin/solidity-service \
          -b 127.0.0.1 -p 3000 \
          ${remixIde}
      '';
          # -r ${machines.riemannA.dns} \
    };

    services.nginx = {
      enable = true;
      user = "eth";
      virtualHosts = {
        "~." = {
          listen = [{ addr = "0.0.0.0"; port = 8080; }];
          extraConfig = ''
            return 301 https://$host$request_uri;
            '';
        };
        "${config.deployment.name}.*" = {
          listen = [{ addr = "0.0.0.0"; port = 80; }];
          locations = {
            "/" = {
              root = "${ethereum-explorer}";
              tryFiles = "$uri /index.html =404";
              extraConfig = "access_log /var/log/nginx-access-explorer.log;";
            };
            "/versions" = {
              alias = "${versions}";
              index = "index.txt";
            };
            "/api/".proxyPass = "http://${mantis-node-ip}:8546/";
            "/healthcheck/" = {
              proxyPass = "http://127.0.0.1:4000/";
              proxyWebsockets = true;
            };
            "/remix/".proxyPass = "http://127.0.0.1:3000/";
          };
        };
      };
    };

    users.users.eth = {
      isNormalUser = true;
      home = "/home/eth";
      description = "Eth explorer user";
      extraGroups = [ "systemd-journal" ];
    };
  };
}
