{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP = true;
      enableEkgWeb      = true;
      dnsDomainname     = "aws.iohkdev.io";
    };

    services = {
      dd-agent.tags              = ["env:staging"];

      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
