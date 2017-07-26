{ accessKeyId, ... }:

with (import ./../lib.nix);
{
  resources.ec2SecurityGroups =
    let config   = (import ./cardano-nodes-config.nix { inherit accessKeyId; });
        SG'list  = flip map config.securityGroupNames
                   (name: { name  = name;
                            value = { resources, ... }: (config.securityGroups (traceF builtins.attrNames resources.elasticIPs))."${name}"; });
    in listToAttrs SG'list;
}
