let topology = import ./../../topology.nix; in
with builtins;
with import ../../lib.nix;
{ accessKeyId, deployerIP, ... }:
let
    org             = "IOHK";
    defaultRegion      = "eu-central-1";
    mantisNodes        = goguenNodes topology "mantis";
    mantisNodesRegions = goguenNodesRegions topology "mantis";
    allAwsRegions         = {
      "a" = defaultRegion;
      "b" = defaultRegion;
      "c" = defaultRegion;
    };
    nodesAwsRegions = map (r: allAwsRegions."${r}") (goguenRegions topology "mantis") ;

    sgByName        = resources: org: region: name:
      resources.ec2SecurityGroups."${name}-${region}-${org}";

    mkMachine = sgs: { resources, ... }: {
      imports = [ <module/amazon-base.nix> ];
      deployment.ec2 = {
        securityGroups = map (sgByName resources org region) (["allow-deployer-ssh"] ++ sgs);
        blockDeviceMapping = {
          "/dev/xvdf" = { size = 120; }; # resources.ebsVolumes."${hostname}-ebs"
        };
        associatePublicIpAddress = true;
      };
    };
    mkExplorer = mkMachine ["allow-explorer-public"];
    mkFaucet   = mkMachine ["allow-faucet-public"];
    mkMantis   = mkMachine ["allow-mantis-public"];
in listToAttrs (map 
      (mantisNode: nameValuePair mantisNode mkMantis)
      (goguenNodes topology "mantis")
  ) // {
  explorer-a = mkExplorer;

  faucet-a   = mkFaucet;

  resources = {
    elasticIPs =  mapAttrs' (node: logicalRegion: nameValuePair "${node}-ip" { 
      inherit accessKeyId;
      region = allAwsRegions."${logicalRegion}"; }
    ) mantisNodesRegions // {
      explorer-a-ip = { inherit  accessKeyId;
        region = defaultRegion;
      };

      faucet-a-ip   = { inherit  accessKeyId;
        region = defaultRegion;
      };
    };

    ec2SecurityGroups =
    let public = type: port: { protocol = type; fromPort = port ; toPort = port ; sourceIp = "0.0.0.0/0"; };
    in {
      "allow-explorer-public-${defaultRegion}-${org}" = { inherit  accessKeyId;
        region = defaultRegion;
        description = "Goguen Explorer public ports";
        rules =  map (public "tcp") [ 80 8080 5601 ]
              ++ map (public "udp") [];
      };
      "allow-faucet-public-${defaultRegion}-${org}" = { inherit  accessKeyId;
        region = defaultRegion;
        description = "Goguen Faucet public ports";
        rules =  map (public "tcp") [ 8099 5555 ]
              ++ map (public "udp") [ 8125 ];
      };
    } // listToAttrs (map (region: nameValuePair "allow-mantis-public-${region}-${org}" {
        inherit region accessKeyId;
        description = "Mantis public ports";
        rules =  map (public "tcp") [ 5555 5679 8546 9076 30303 ]
              ++ map (public "udp") [ 8125 ];
      }) nodesAwsRegions);
    };
  };
}
