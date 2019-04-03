{
  require = [ ./monitoring.nix ];
  monitoring = import ../modules/production.nix;
}

