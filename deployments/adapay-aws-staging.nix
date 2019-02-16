{ environment ? "staging"
, deployerIP ? ""
}:
let
  accessKeyId = "adapay";
  region = "eu-central-1";
  zone = "eu-central-1a";
in {
  importer = { config, pkgs, lib, resources, ... }: {
    ec2.hvm = true;

    deployment = {
      ec2 = {
        inherit accessKeyId region zone;
        ebsInitialRootDiskSize = 20;
        instanceType = "t2.medium";
        keyPair = resources.ec2KeyPairs.adapayKey;
        subnetId = resources.vpcSubnets.adapayVPCSubnet;
        associatePublicIpAddress = true;
        securityGroupIds = [ resources.ec2SecurityGroups.adapaySG.name ];
      };
      targetEnv = "ec2";
      route53 = {
        inherit accessKeyId;
        hostname = "${environment}.adapay.iohk.io";
      };
    };

    # Don't reconfigure the system from EC2 userdata on next startup
    systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
  };
  adapay = { config, pkgs, lib, resources, ... }: {
    ec2.hvm = true;

    deployment = {
      ec2 = {
        inherit accessKeyId region zone;
        ebsInitialRootDiskSize = 20;
        instanceType = "t2.medium";
        keyPair = resources.ec2KeyPairs.adapayKey;
        subnetId = resources.vpcSubnets.adapayVPCSubnet;
        associatePublicIpAddress = true;
        securityGroupIds = [ resources.ec2SecurityGroups.adapaySG.name ];
      };
      targetEnv = "ec2";
      route53 = {
        inherit accessKeyId;
        hostname = "${environment}.adapay.iohk.io";
      };
    };

    # Don't reconfigure the system from EC2 userdata on next startup
    systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
  };
  nginx = { config, pkgs, lib, resources, ... }: {
    ec2.hvm = true;

    deployment = {
      ec2 = {
        inherit accessKeyId region zone;
        ebsInitialRootDiskSize = 20;
        instanceType = "t2.medium";
        keyPair = resources.ec2KeyPairs.adapayKey;
        elasticIPv4 = resources.elasticIPs.adapayIP;
        subnetId = resources.vpcSubnets.adapayVPCSubnet;
        associatePublicIpAddress = true;
        securityGroupIds = [ resources.ec2SecurityGroups.adapaySG.name ];
      };
      targetEnv = "ec2";
      route53 = {
        inherit accessKeyId;
        hostname = "${environment}.adapay.iohk.io";
      };
    };

    # Don't reconfigure the system from EC2 userdata on next startup
    systemd.services.amazon-init.wantedBy = lib.mkForce [ ];
  };
  resources = {
    elasticIPs.adapayIP = {
      inherit accessKeyId region;
      vpc = true;
    };
    ec2KeyPairs.adapayKey = {
      inherit accessKeyId region;
    };
    ec2SecurityGroups.adapaySG = { resources, ... }: {
      inherit accessKeyId region;
      vpcId = resources.vpc.adapayVPC;
      rules = let
        allowPortSingle = source: port: {
            fromPort = port;
            toPort = port;
            sourceIp = source;
        };
        allowPortPublic = allowPortSingle "0.0.0.0/0";
      in [
        (allowPortSingle deployerIP 22)
        ] ++
        (map allowPortPublic [ 80 443 22 ]);
    };
    vpc.adapayVPC = {
      inherit accessKeyId region;
      enableDnsSupport = true;
      cidrBlock = "10.0.0.0/16";
    };
    vpcSubnets.adapayVPCSubnet = { resources, ... }: {
      inherit accessKeyId region zone;
      cidrBlock = "10.0.1.0/24";
      vpcId = resources.vpc.adapayVPC;
      mapPublicIpOnLaunch = true;
    };
    vpcInternetGateways.adapayIGW = { resources, ... }: {
      inherit region accessKeyId;
      vpcId = resources.vpc.adapayVPC;
    };

    vpcRouteTables.adapayRouteTable =  { resources, ... }: {
      inherit region accessKeyId;
      vpcId = resources.vpc.adapayVPC;
    };

    vpcRouteTableAssociations.adapayAssociation = { resources, ... }: {
      inherit region accessKeyId;
      subnetId = resources.vpcSubnets.adapayVPCSubnet;
      routeTableId = resources.vpcRouteTables.adapayRouteTable;
    };

    vpcRoutes.adapayIGWRoute = { resources, ... }: {
      inherit region accessKeyId;
      routeTableId = resources.vpcRouteTables.adapayRouteTable;
      destinationCidrBlock = "0.0.0.0/0";
      gatewayId = resources.vpcInternetGateways.adapayIGW;
    };
  };
}
