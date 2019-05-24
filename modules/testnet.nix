{ name, config, resources, ... }:

with import ../lib.nix;
{
  config = {

    global = {
      allocateElasticIP = true;
      enableEkgWeb      = false;
    };

    services = {

      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;

      monitoring-exporters.enable = true;
      monitoring-exporters.metrics = true;
      monitoring-exporters.logging = true;
    };
  };
}
