{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP = false;
      enableEkgWeb      = true;
      dnsDomainname     = null;
    };

    services = {
      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
