{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP            = false;
      dnsDomainname                = "awstest2.iohkdev.io";
      omitDetailedSecurityGroups   = true;
    };

    services = {
      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
