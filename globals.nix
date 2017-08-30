{ IOHKaccessKeyId, CFaccessKeyId, SGGaccessKeyId
, deployerIP
, topologyYaml                  ## The original stuff we're passing on to the nodes.
, topologyFile ? ./topology.nix ## The iohk-ops post-processed thing.
, systemStart
, environment
, ... }:

with (import ./lib.nix);

let topologySpec     = builtins.fromJSON (builtins.readFile topologyFile);
    # WARNING: this sort order is key in a number of things:
    #          - relay numbering
    #          - DNS naming
    topologySpecList = (builtins.sort (l: r: l.name < r.name)
                                      (mapAttrsToList (k: v: { name = k; value = v;}) topologySpec))
                       ++ [ explorerSpecElt reportServerSpecElt ];
    # NOTE: the following definitions for explorerSpecElt and reportServerSpecElt
    #       allow us to treat all cluster nodes in a uniform way.
    #       It's as if they were defined in the topology yaml.
    explorerSpecElt  = { name  = "explorer";
                         value = { org      = defaultOrg;
                                   region   = centralRegion;
                                   type     = "other";
                                   kademlia = false;
                                   peers    = [];
                                   address  = "explorer.cardano";
                                   port     = 3000; }; };
    reportServerSpecElt =
                       { name  = "report-server";
                         value = { org      = defaultOrg;
                                   region   = centralRegion;
                                   type     = "other";
                                   kademlia = false;
                                   peers    = [];
                                   address  = "report-server.cardano";
                                   port     = 8080; }; };

    allRegions     = unique ([centralRegion] ++ map (n: n.value.region) topologySpecList);
    centralRegion  = "eu-central-1";

    allOrgs        = [ "IOHK" "CF" "SGG" ];
    defaultOrg     =   "IOHK";
    orgAccessKeys  = {  IOHK = IOHKaccessKeyId; CF = CFaccessKeyId; SGG = SGGaccessKeyId; };

    ## All actual (Region * Org) pairs.
    orgXRegions    = unique (flip map topologySpecList
                     (x: { region = x.value.region; org = x.value.org; }));

    indexed        = imap (n: x:
            { name = x.name;
             value = rec {
                  inherit (x.value) org region kademlia peers address port;
                                i = n - 1;
                             name = x.name;       # This is an important identity, let's not break it.
                         nodeType = x.value.type;
                       typeIsCore = nodeType == "core";
                      typeIsRelay = nodeType == "relay";
                   typeIsExplorer = name == "explorer";
                typeIsRunsCardano = typeIsCore || typeIsRelay || typeIsExplorer;
               typeIsReportServer = name == "report-server";
                      accessKeyId = if elem org allOrgs
                                    then orgAccessKeys.${org}
                                    else throw "Node '${name}' has invalid org '${org}' specified -- must be one of: ${toString allOrgs}.";
                      keyPairName = orgRegionKeyPairName org region;
                       relayIndex = if typeIsRelay then i - firstRelayIndex else null;
                                    ## For the SG definitions look below in this file:
                          sgNames = [ "allow-deployer-ssh-${region}-${org}" ]
                                    ++ optionals typeIsExplorer     [ "allow-to-explorer-${region}" ]
                                    ++ optionals typeIsReportServer [ "allow-to-report-server-${region}" ]
                                    ++ optionals typeIsCore         [ "allow-cardano-static-peers-${name}-${region}-${org}" ]
                                    ++ optionals typeIsRelay        [ "allow-kademlia-public-udp-${region}"
                                                                      "allow-cardano-public-tcp-${region}" ]
                                    ++ optionals typeIsRunsCardano  [ ];
                             }; } )
                     topologySpecList;
    ## Summary:
    ##
    cores           = filter     (x: x.value.typeIsCore)              indexed;
    relays          = filter     (x: x.value.typeIsRelay)             indexed;
    nodeMap         = listToAttrs (cores ++ relays);
    # WARNING: this depends on the sort order, as explained above.
    firstRelay      = findFirst (x: x.value.typeIsRelay) (throw "No relays in topology, it's sad we can't live, but then.. who does?") indexed;
    firstRelayIndex = firstRelay.value.i;
    nRelays         = length relays;
    ## Fuller map to include "other" nodes:
    ##
    explorerNV      = findFirst  (x: x.value.typeIsExplorer)     {}   indexed;
    reportServerNV  = findFirst  (x: x.value.typeIsReportServer) {}   indexed;
    fullMap'        = nodeMap // listToAttrs (builtins.filter (x: x != {})
                                              [ explorerNV reportServerNV ]);
    fullMap         = mapAttrs (k: v: traceDSF id v) fullMap';

    orgRegionKeyPairName = org: region: "cardano-keypair-${org}-${region}";

    ## allKeyPairs :: Map KeypairName Keypair
    allKeyPairs     = listToAttrs (flip map orgXRegions
                                   ({ org, region }:
                                    nameValuePair (orgRegionKeyPairName org region)
                                                  { inherit region; accessKeyId = orgAccessKeys.${org}; }));
in
{
  inherit topologyYaml;
  inherit cores relays nodeMap fullMap;
  inherit nRelays firstRelayIndex;
  inherit allRegions centralRegion;
  inherit allOrgs defaultOrg;
  inherit orgXRegions;
  inherit orgAccessKeys;
  inherit allKeyPairs;
  ###
  inherit deployerIP systemStart environment;
}
