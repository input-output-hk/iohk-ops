{ accessKeyId, deployerIP
, topologyFile ? ./../topology.nix
, ... }:

with (import ./../lib.nix);

let topologySpec  = (builtins.fromJSON (builtins.readFile topologyFile));
    topologyList  = builtins.sort (l: r: l.name < r.name)
                                   (mapAttrsToList (k: v: { name = k; value = v; }) topologySpec);
    regionList    = unique (map (n: n.value.region) topologyList);
    indexed       = imap (n: x:
           let spec = x.value; in
           { name = x.name; value = rec {
                                        inherit (spec) region type kademlia peers;
                                i = n - 1;
                             name = x.name; # This is an important identity, let's not break it.
                         sg-names = if      spec.type == "other" then
                                      [ "allow-open-${region}" ]
                                    else if spec.type == "core"  then
                                      [ "allow-deployer-ssh-${region}"
                                        "allow-cardano-static-peers-${name}-${region}" ]
                                    else if spec.type == "relay" then
                                      [ "allow-deployer-ssh-${region}"
                                        "allow-kademlia-public-udp-${region}"
                                        "allow-cardano-public-tcp-${region}" ]
                                    else throw "While computing EC2 SGs: unhandled cardano-node type: '${type}'";
                             }; } ) topologyList;
    byName        = name: let xs = filter (n: n.name == name) indexed;
                          in if xs != [] then builtins.elemAt xs 0
                             else throw "No indexedNode by name '${name}'.";
    # Canonical node parameter format is:
    #   i             :: Int
    #   region        :: String
    #   type          :: String               -- one of: 'core', 'relay'
    #   static-routes :: [['nodeId, 'nodeId]] -- here we go, TupleList..
    canonical       = builtins.listToAttrs indexed;
    cores           = filter (x: x.value.type == "core")  indexed;
    relays          = filter (x: x.value.type == "relay") indexed;
    ##
    ##
    regionSGNames = region:
        [ "allow-open-${region}"
          "allow-deployer-ssh-${region}"
          "allow-kademlia-public-udp-${region}"
          "allow-cardano-public-tcp-${region}"
        ];
    regionSGs      = nodePort: region: {
        "allow-open-${region}" = {
          inherit region accessKeyId;
          description = "Everything goes";
          rules = [{
            protocol = "-1"; # All traffic
            sourceIp = "0.0.0.0/0";
            fromPort = 0; toPort = 65535;
          }];
        };
        "allow-deployer-ssh-${region}" = {
          inherit region accessKeyId;
          description = "SSH";
          rules = [{
            protocol = "6"; # TCP
            fromPort = 22; toPort = 22;
            sourceIp = deployerIP + "/32";
          }];
        };
        "allow-kademlia-public-udp-${region}" = {
          inherit region accessKeyId;
          description = "Kademlia UDP public";
          rules = [{
            protocol = "17"; # UDP
            sourceIp = "0.0.0.0/0";
            fromPort = 1024; toPort = 65535;
          }];
        };
        "allow-cardano-public-tcp-${region}" = {
          inherit region accessKeyId;
          description = "Cardano TCP public";
          rules = [{
            protocol = "6"; # TCP
            fromPort = nodePort; toPort = nodePort;
            sourceIp = "0.0.0.0/0";
          }];
        };
      };
    coreSGNames = core:
        [ "allow-cardano-static-peers-${core.name}-${core.value.region}" ];
    coreSGs     = nodePort: ips: core:
      let neighbourNames = traceSF (p: "${core.name} peers: " + concatStringsSep ", " core.value.peers) core.value.peers;
          neighbours = map byName neighbourNames;
          neigh'rule =
          neigh:
          let
            ip = ips."${toString neigh.value.name}-ip";
          in {
              fromPort = nodePort; toPort = nodePort;
              sourceIp = traceSF (x: "${core.name} allowing peer ${x.address}") ip;
            };
      in {
        "allow-cardano-static-peers-${core.name}-${core.value.region}" = {
          inherit accessKeyId;
          region = core.value.region;
          description = "Cardano TCP static peers of ${core.name}";
          rules = map neigh'rule neighbours;
        };
      };
    securityGroupNames =
     (    concatMap  regionSGNames                             regionList
      ++  concatMap  coreSGNames                               cores
     );
    elasticIPsSecurityGroups =
     elasticIPs:
     (fold (x: y: x // y) {}
     (    map       (regionSGs     cconf.nodePort)             regionList
      ++  map       (coreSGs       cconf.nodePort elasticIPs)  cores
     ));
in
{
  nodeArgs           = canonical;
  cores              = cores;
  relays             = relays;
  securityGroupNames = securityGroupNames;
  securityGroups     = elasticIPsSecurityGroups;
}
