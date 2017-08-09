# TODO: get rid of this duplication between config.nix and modules/cardano-node.nix
# XXX: rename this file:  cardano-node-config vs. cardano-nodes-config is CRUEL
with (import ./../lib.nix);

params:
  { pkgs, nodes, config, ...}:
  let
    neighbourNames       = flatten params.static-routes;
    nodeNameToPublicIP   = name: cardanoAttr "publicIP" nodes.${name}.config.services.cardano-node;
    neighbourPairs       = map (n: { name = n; ip = nodeNameToPublicIP n; })
                               (builtins.trace "${params.name}: role '${params.type}'" neighbourNames);
    ppNeighbour          = n: "${n.name}: ${n.ip}";
    sep                  = ", ";
  in {
    imports = [
      ./common.nix
    ];

    services.dnsmasq.enable = true;
    services.dnsmasq.servers = [ "127.0.0.1" ];

    networking.extraHosts =
    let hostList = if config.services.cardano-node.enable == false then []
                   else
                   neighbourPairs
                   ++ (map (x:
                        { name = x.name;
                          ip   = nodeNameToPublicIP x.name; })
                        (filter (x: x.name != params.name) params.relays))
                   ++ (if !(cardanoHasAttr "publicIP" config.services.cardano-node) then []
                       else [ { name = params.name;
                                ip   = nodeNameToPublicIP params.name; } ]);
    in
    if params ? noExtraHosts && params.noExtraHosts then "" else
    ''
    ${concatStringsSep "\n" (map (host: "${host.ip} ${host.name}.cardano") hostList)}
    '';

    services.cardano-node = {
      enable     = true;
      nodeName   = params.name;
      type       = params.type;
      nodeIndex  = params.i;
      port       = cconf.nodePort;
      inherit (cconf) enableP2P genesisN slotDuration networkDiameter mpcRelayInterval;
      inherit (cconf) totalMoneyAmount bitcoinOverFlat productionMode systemStart richPoorDistr;
      neighbours = builtins.trace "${params.name}: neighbours: ${concatStringsSep sep (map ppNeighbour neighbourPairs)}"
                                  neighbourPairs;
    };
  }
