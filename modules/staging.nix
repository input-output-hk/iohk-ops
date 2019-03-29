{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP = true;
      enableEkgWeb      = true;
      dnsDomainname     = "awstest.iohkdev.io";
    };

    services = {

      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
