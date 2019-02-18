{ configFile ? <config/default.nix>
, accessKeyId, ... }:
with import configFile; {
  hydra.imports = [ <module/hydra-aws.nix> ];

  resources.elasticIPs.hydra-ip = { inherit region accessKeyId; };
  resources.ec2SecurityGroups = {
    "allow-hydra-${region}-${org}" = { resources, ...}: {
      inherit region accessKeyId;
      description = "SSH";
      rules = [
        {
          protocol = "tcp"; # TCP
          fromPort = 80; toPort = 80;
          sourceIp = "0.0.0.0/0";
        }{
          protocol = "tcp"; # TCP
          fromPort = 443; toPort = 443;
          sourceIp = "0.0.0.0/0";
        }{
          protocol = "tcp"; # TCP
          fromPort = 22; toPort = 22;
          sourceIp = resources.elasticIPs.hydra-ip;
        }];
    };
  };
}
