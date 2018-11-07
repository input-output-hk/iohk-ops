{ ... }:

{
  imports = [
    ./log-classifier-service.nix
  ];

  services.log-classifier = {
    enable = true;
    secrets = builtins.toFile "log-classifier-env" ''
      LCPATH=/var/run/keys
    '';
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  systemd.services.log-classifier.after = [ "keys.target" ];
  systemd.services.log-classifier.requires = [ "keys.target" ];
  users.users.log-classifier.extraGroups = [ "keys" ];

  deployment.keys = {
    "app_users.json" = {
      keyFile = ../static/log-classifier/app_users.json;
      user = "log-classifier";
    };
    token = {
      keyFile = ../static/log-classifier/zendesk_token.secret;
      user = "log-classifier";
    };
    assign_to = {
      keyFile = ../static/log-classifier/assign_to;
      user = "log-classifier";
    };
    "knowledge.csv" = {
      keyFile = ../static/log-classifier/knowledge.csv;
      user = "log-classifier";
    };
  };
}
