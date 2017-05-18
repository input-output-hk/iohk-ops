with (import ./../lib.nix);

{
  resources = {
    inherit ec2KeyPairs;
  };
}
