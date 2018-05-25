{ config, lib, pkgs, ... }:

{
  # dd-agent module is not in nix-darwin upstream
  imports = [ ../services/dd-agent/dd-agent.nix ];

  services.dd-agent = {
    enable = true;
    apiKeyFile = "/Users/admin/.datadog_api_key";
  };

  users.knownUsers = [ "datadog" ];
  users.knownGroups = [ "datadog" ];
  users.users.datadog = {
    uid = 533;
    gid = 533;
  };
  users.groups.datadog.gid = 533;
  system.activationScripts.postActivation.text = ''
    mkdir -p /var/log/datadog
    chmod 770 /var/log/datadog
    chown datadog:datadog /var/log/datadog
  '';
}
