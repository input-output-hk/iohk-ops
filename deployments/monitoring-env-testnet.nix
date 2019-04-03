{
  require = [ ./monitoring.nix ];
  monitoring = import ../modules/testnet.nix;
}