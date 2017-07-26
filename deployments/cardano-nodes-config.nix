{ accessKeyId, ... }:

with (import ./../lib.nix);

let cluster'spec   = (builtins.fromJSON (builtins.readFile ./../cluster.nix)).nodes; # Strip the outer "nodes:" shell of cluster.yaml
    cluster'list   = builtins.sort (l: r: l.name < r.name)
                                   (mapAttrsToList (k: v: { name = k; value = v; }) cluster'spec);
    region'list    = unique (map (n: n.value.region) cluster'list);
    indexed        = imap (i: x: with x.value;
          { name = x.name; value = {
                               i = i - 1;
                            name = x.name; # This is an important identity, let's not break it.
                          region = region;
                            type = type;
                   static-routes = if builtins.hasAttr "static-routes" x.value then x.value.static-routes else [];
                             }; } ) cluster'list;
    by'name        = name: let xs = filter (n: n.name == name) indexed;
                           in if xs != [] then builtins.elemAt xs 0
                              else throw "No indexed'node by name '${name}'.";
    # Canonical node parameter format is:
    #   i             :: Int
    #   region        :: String
    #   type          :: String             -- one of: 'core', 'relay'
    #   static-routes :: [['nodeId, 'nodeId]] -- here we go, TupleList..
    canonical       = builtins.listToAttrs indexed;
    cores           = filter (x: x.value.type == "core") indexed;
    ##
    ##
    region'open'SG'name = region:
        "allow-open-${region}";
    region'open'SG      = region: {
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
    region'ssh'SG'name = region:
        "allow-ssh-${region}";
    region'ssh'SG      = region: {
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
    region'kademlia'public'tcp'SG'name = region:
        "allow-kademlia-public-udp-${region}";
    region'kademlia'public'tcp'SG      = region: {
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
    region'cardano'public'tcp'SG'name = region:
        "allow-cardano-public-tcp-${region}";
    region'cardano'public'tcp'SG      = port: region: {
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
    core'cardano'static'peers'SG'name = core:
        "allow-cardano-static-peers-${core.name}-${core.value.region}";
    core'cardano'static'peers'SG      = port: ips: core:
      let neighbour'names = flatten core.value.static-routes;
          neighbours = map by'name neighbour'names;
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
    security'group'names =
     (    map  region'open'SG'name                                      region'list
      ++  map  region'ssh'SG'name                                       region'list
      ++  map  core'cardano'static'peers'SG'name                        cores
      ++  map  region'kademlia'public'tcp'SG'name                       region'list
      ++  map  region'cardano'public'tcp'SG'name                        region'list
     );
    elastic'ips'security'groups =
     elastic'ips:
     (fold (x: y: x // y) {}
     (    map  region'open'SG                                           region'list
      ++  map  region'ssh'SG                                            region'list
      ++  map (core'cardano'static'peers'SG cconf.nodePort elastic'ips) cores
      ++  map  region'kademlia'public'tcp'SG                            region'list
      ++  map (region'cardano'public'tcp'SG cconf.nodePort)             region'list
     ));
in
{
  nodeArgs           = canonical;
  securityGroupNames = security'group'names;
  securityGroups     = elastic'ips'security'groups;
}
