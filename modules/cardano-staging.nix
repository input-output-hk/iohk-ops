with (import ./../lib.nix);

params:
{
  imports = [
    ./staging.nix
    ./monitoring-exporters.nix
  ];

  global.dnsHostname = if params.typeIsRelay then "cardano-node-${toString params.relayIndex}" else null;

  deployment.ec2.instanceType =
    mkForce (if (params.nodeImpl == "haskell")
             then "t3.xlarge"
             else "t2.large");
}
