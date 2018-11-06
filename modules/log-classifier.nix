{ ... }:

{
  imports = [
    ./log-classifier-service.nix
  ];

  services.log-classifier = {
    enable = true;
    secrets = builtins.toFile "log-classifier-env" ''
      LCPATH=/var/lib/keys
    '';
  };
  systemd.services.log-classifier.after = [ "keys.target" ];
  systemd.services.log-classifier.requires = [ "keys.target" ];
  users.users.log-classifier.extraGroups = [ "keys" ];

  deployment.keys = {
    app_users = {
      keyFile = ../static/log-classifier/app_users.json;
      user = "log-classifier";
    };
    zendesk_token = {
      keyFile = ../static/zendesk_token.secret;
      user = "log-classifier";
    };
    assign_to = {
      keyFile = ../static/log-classifier/assign_to;
      user = "log-classifier";
    };
    knowledge = {
      keyFile = ../static/log-classifier/knowledge.csv;
      user = "log-classifier";
    };
  };
}
