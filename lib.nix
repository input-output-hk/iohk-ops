let
  lib = (import <nixpkgs> {}).lib;
in lib // {
  # Allows to also generate the key compared to upstream genAttrs
  genAttrs' = names: fkey: fname:
    lib.listToAttrs (map (n: lib.nameValuePair (fkey n) (fname n)) names);

  # If we're generating an AMI, don't set nixops deployment attributes
  generatingAMI = builtins.getEnv "GENERATING_AMI";
}
