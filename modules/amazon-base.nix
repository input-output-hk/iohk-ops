{ name, config, resources, ... }:

with (import ./../lib.nix);

{
 deployment.targetEnv = "ec2";
 deployment.ec2.instanceType = mkDefault "t2.large";
 deployment.ec2.region = mkDefault region;
 deployment.ec2.keyPair = mkDefault (resources.ec2KeyPairs.${keypairFor config.deployment.ec2.accessKeyId region});
 deployment.ec2.ebsInitialRootDiskSize = mkDefault 30;
 networking.hostName = "${config.deployment.name}.${config.deployment.targetEnv}.${name}";

 ## See deployments/cardano-nodes-config.nix

 deployment.ec2.securityGroups =
   let
     region = config.deployment.ec2.region;
     name   = config.services.cardano-node.nodeName;
     type   = config.services.cardano-node.type;
   in if   config.services.cardano-node.enable == false
        || type == "other" then
        mkDefault [ resources.ec2SecurityGroups."allow-open-${region}" ]
   else if type == "core"  then
        mkForce   [ resources.ec2SecurityGroups."allow-ssh-${region}"
                    resources.ec2SecurityGroups."allow-cardano-static-peers-${name}-${region}" ]
   else if type == "relay" then
        mkForce   [ resources.ec2SecurityGroups."allow-ssh-${region}"
                    resources.ec2SecurityGroups."allow-kademlia-public-udp-${region}"
                    resources.ec2SecurityGroups."allow-cardano-public-tcp-${region}" ]
   else throw "While computing EC2 SGs: unhandled cardano-node type: '${type}'";
}
