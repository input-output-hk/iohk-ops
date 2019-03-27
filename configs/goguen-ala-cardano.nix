{
  node = {
    org                  = "IOHK";
    region               = "eu-central-1";
  };

  cluster = {
    hostedZone           = "project42.iohkdev.io";
    allocateElasticIP    = true;
    oauthEnable          = true;
  };
}
