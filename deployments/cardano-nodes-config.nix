{ accessKeyId, ... }:

with (import ./../lib.nix);

let clusterSpec   = (builtins.fromJSON (builtins.readFile ./../cluster.nix)).nodes; # Strip the outer "nodes:" shell of cluster.yaml
    clusterList   = builtins.sort (l: r: l.name < r.name)
                                   (mapAttrsToList (k: v: { name = k; value = v; }) clusterSpec);
    regionList    = unique (map (n: n.value.region) clusterList);
    indexed       = imap (i: x: with x.value;
           { name = x.name; value = {
                                i = i - 1;
                             name = x.name; # This is an important identity, let's not break it.
                           region = region;
                             type = type;
                    static-routes = if builtins.hasAttr "static-routes" x.value then x.value.static-routes else [];
                             }; } ) clusterList;
    byName        = name: let xs = filter (n: n.name == name) indexed;
                          in if xs != [] then builtins.elemAt xs 0
                             else throw "No indexedNode by name '${name}'.";
    # Canonical node parameter format is:
    #   i             :: Int
    #   region        :: String
    #   type          :: String               -- one of: 'core', 'relay'
    #   static-routes :: [['nodeId, 'nodeId]] -- here we go, TupleList..
    canonical       = builtins.listToAttrs indexed;
    cores           = filter (x: x.value.type == "core") indexed;
    ##
    ##
    regionOpenSGName = region:
        "allow-open-${region}";
    regionOpenSG      = region: {
        "allow-open-${region}" = {
          inherit region accessKeyId;
          description = "Everything goes";
          rules = [{
            protocol = "-1"; # All traffic
            sourceIp = "0.0.0.0/0";
            fromPort = 0;
            toPort   = 65535;
          }];
        };
      };
    regionSshSGName = region:
        "allow-ssh-${region}";
    regionSshSG      = region: {
        "allow-ssh-${region}" = {
          inherit region accessKeyId;
          description = "SSH";
          rules = [{
            fromPort = 22;
            toPort   = 22;
            sourceIp = "0.0.0.0/0"; # TODO: move ssh to use $SMART_GEN_IP
          }];
        };
      };
    regionKademliaPublicTcpSGName = region:
        "allow-kademlia-public-udp-${region}";
    regionKademliaPublicTcpSG      = region: {
        "allow-kademlia-public-udp-${region}" = {
          inherit region accessKeyId;
          description = "Kademlia UDP public";
          rules = [{
            protocol = "17"; # UDP
            sourceIp = "0.0.0.0/0";
            fromPort = 1024;
            toPort   = 65535;
          }];
        };
      };
    regionCardanoPublicTcpSGName = region:
        "allow-cardano-public-tcp-${region}";
    regionCardanoPublicTcpSG      = port: region: {
        "allow-cardano-public-tcp-${region}" = {
          inherit region accessKeyId;
          description = "Cardano TCP public";
          rules = [{
            fromPort = port;
            toPort   = port;
            sourceIp = "0.0.0.0/0";
          }];
        };
      };
    coreCardanoStaticPeersSGName = core:
        "allow-cardano-static-peers-${core.name}-${core.value.region}";
    coreCardanoStaticPeersSG      = port: ips: core:
      let neighbourNames = flatten core.value.static-routes;
          neighbours = map byName neighbourNames;
          neigh'rule =
          neigh:
          let
            ip = ips."nodeip${toString neigh.value.i}".address;
          in {
              fromPort = port;
              toPort   = port;
              sourceIp = ip + "/32";
            };
      in {
        "allow-cardano-static-peers-${core.name}-${core.value.region}" = {
          inherit region accessKeyId;
          description = "Cardano TCP static peers of ${core.name}";
          rules = map neigh'rule neighbours;
        };
      };
    securityGroupNames =
     (    map  regionOpenSGName                                     regionList
      ++  map  regionSshSGName                                      regionList
      ++  map  coreCardanoStaticPeersSGName                         cores
      ++  map  regionKademliaPublicTcpSGName                        regionList
      ++  map  regionCardanoPublicTcpSGName                         regionList
     );
    elasticIPsSecurityGroups =
     elasticIPs:
     (fold (x: y: x // y) {}
     (    map  regionOpenSG                                         regionList
      ++  map  regionSshSG                                          regionList
      ++  map (coreCardanoStaticPeersSG cconf.nodePort elasticIPs)  cores
      ++  map  regionKademliaPublicTcpSG                            regionList
      ++  map (regionCardanoPublicTcpSG cconf.nodePort)             regionList
     ));
in
{
  nodeArgs           = canonical;
  securityGroupNames = securityGroupNames;
  securityGroups     = elasticIPsSecurityGroups;
}
