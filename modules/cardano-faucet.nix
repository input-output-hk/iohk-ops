with (import ./../lib.nix);

globals: params:
{ config, pkgs, lib, ...}:

{
  global.dnsHostname = mkForce "cardano-faucet";

  imports = [ ./cardano-faucet-module.nix ];

  services.cardano-faucet = {
    enable = true;
    relayHost = "relays.cardano-testnet.iohkdev.io";
    configKey = "testnet_full";
    statsd.enable = true;
    statsd.port = 8125; # dogstatsd will be listening on this port
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
