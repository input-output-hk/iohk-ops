let
  lib = (import <nixpkgs> {}).lib;
in lib // {
  # Allows to also generate the key compared to upstream genAttrs
  genAttrs' = names: fkey: fname:
    lib.listToAttrs (map (n: lib.nameValuePair (fkey n) (fname n)) names);

  # If we're generating an AMI, don't set nixops deployment attributes
  generatingAMI = builtins.getEnv "GENERATING_AMI";

  # Function to generate DHT key
  genDhtKey = { i
              , dhtKeyPrefix  ? "MHdrsP-oPf7UWly"
              , dhtKeyPostfix ? "7QuXnLK5RD=" }:
              let padded =
                  if i < 10
                  then "0" + toString i
                  else toString i
              ; in dhtKeyPrefix + padded + dhtKeyPostfix;
}
