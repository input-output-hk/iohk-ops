{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP = true;
      enableEkgWeb      = false;
      dnsDomainname     = "aws.iohk.io";
    };

    services = {
      dd-agent.tags              = ["env:${config.deployment.name}"];

      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
