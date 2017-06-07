{ config, pkgs, lib, ... }:

with (import ./../lib.nix);

{
  config = {
    services.dd-agent = {
      enable = true;
      api_key = apiKey;
      hostname = config.networking.hostName;
    };
  };
}
