{ name, config, resources, ... }:

with import ./../lib.nix;
{
  config = {

    global = {
      allocateElasticIP            = false;
      enableEkgWeb                 = true;
      dnsHostname                  = null;
      dnsDomainname                = mkForce null;
      omitDetailedSecurityGroups   = true;
    };

    services = {
      # DEVOPS-64: disable log bursting
      journald.rateLimitBurst    = 0;
    };

  };
}
