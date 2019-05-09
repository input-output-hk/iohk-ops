{
  node = {
    org                  = "IOHK";
    region               = "eu-central-1";
  };

  cluster = {
    hostedZone           = "iohkdev.io";
    allocateElasticIP    = true;
    oauthEnable          = true;
  };
}
