## Do not change this file.
##
## Instead, commit its copy into repository and aim your deployment to it using
## the 'configFile' network argument.  You can also drop the comment from the copy : -)
##
## NOTE: this file is meant to be available for both machine and resource definitions.
##       Unfortunately, easy sharing of it across the deployment spec is currently impossible,
##       so a complex approach is needed:
##
##       1. ./deployment/config.nix MUST forward all values defined here that are intended
##          for per-machine consumption, by feeding that through an option appropriately
##          defined in ./modules/parameters.nix
##
##       2. ./deployment/config.nix MUST effectuate all values that are expected to have
##          influence on resources, that are deployment-independent.
##
##       3. For the (unfortunate) remainder of cases, where resource-based interpretation
##          depends on the deployment, the particular deployment must import this file:
##
##          import <config>
{
  ## Default values for organisation and region.
  org           = "IOHK";
  region        = "eu-central-1";

  ## Do not allocate hosted zones by default.
  hostedZone    = null;
}
