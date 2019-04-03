{
  require = [ ./monitoring.nix ];
  monitoring = import ../modules/staging.nix;
}