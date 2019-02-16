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
        ebsInitialRootDiskSize = 200;
        instanceType = "t2.medium";
        keyPair = resources.ec2KeyPairs.adapayKey;
        subnetId = resources.vpcSubnets.adapayVPCSubnet;
        associatePublicIpAddress = true;
        securityGroupIds = [
          resources.ec2SecurityGroups.adapaySG.name
          resources.ec2SecurityGroups.adapaySGimporter.name
        ];
        tags = { inherit environment; };
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
        securityGroupIds = [
          resources.ec2SecurityGroups.adapaySG.name
          resources.ec2SecurityGroups.adapaySGadapay.name
        ];
        tags = { inherit environment; };
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
        securityGroupIds = [
          resources.ec2SecurityGroups.adapaySG.name
          resources.ec2SecurityGroups.adapaySGnginx.name
        ];
        tags = { inherit environment; };
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
      tags = { inherit environment; };
    };
    ec2KeyPairs.adapayKey = {
      inherit accessKeyId region;
      tags = { inherit environment; };
    };
    ec2SecurityGroups = let
      allowPortSource = source: port: {
          fromPort = port;
          toPort = port;
          sourceIp = source;
      };
      allowPortPublic = allowPortSource "0.0.0.0/0";
      allowPortVPC = allowPortSource "10.0.0.0/16";
    in {
      adapaySG = { resources, ... }: {
        inherit accessKeyId region;
        vpcId = resources.vpc.adapayVPC;
        rules = map allowPortPublic [ 22 ];
        tags = { inherit environment; };
      };
      adapaySGnginx = { resources, ... }: {
        inherit accessKeyId region;
        vpcId = resources.vpc.adapayVPC;
        rules = map allowPortPublic [ 80 443 ];
        tags = { inherit environment; };
      };
      adapaySGimporter = { resources, ... }: {
        inherit accessKeyId region;
        vpcId = resources.vpc.adapayVPC;
        rules = [ (allowPortVPC 8200) ];
        tags = { inherit environment; };
      };
      adapaySGadapay = { resources, ... }: {
        inherit accessKeyId region;
        vpcId = resources.vpc.adapayVPC;
        rules = [ (allowPortVPC 8081) ];
        tags = { inherit environment; };
      };
    };
    vpc.adapayVPC = {
      inherit accessKeyId region;
      enableDnsSupport = true;
      cidrBlock = "10.0.0.0/16";
      tags = { inherit environment; };
    };
    vpcSubnets.adapayVPCSubnet = { resources, ... }: {
      inherit accessKeyId region zone;
      cidrBlock = "10.0.1.0/24";
      vpcId = resources.vpc.adapayVPC;
      mapPublicIpOnLaunch = true;
      tags = { inherit environment; };
    };
    vpcInternetGateways.adapayIGW = { resources, ... }: {
      inherit region accessKeyId;
      vpcId = resources.vpc.adapayVPC;
      tags = { inherit environment; };
    };

    vpcRouteTables.adapayRouteTable =  { resources, ... }: {
      inherit region accessKeyId;
      vpcId = resources.vpc.adapayVPC;
      tags = { inherit environment; };
    };

    vpcRouteTableAssociations.adapayAssociation = { resources, ... }: {
      inherit region accessKeyId;
      subnetId = resources.vpcSubnets.adapayVPCSubnet;
      routeTableId = resources.vpcRouteTables.adapayRouteTable;
      tags = { inherit environment; };
    };

    vpcRoutes.adapayIGWRoute = { resources, ... }: {
      inherit region accessKeyId;
      routeTableId = resources.vpcRouteTables.adapayRouteTable;
      destinationCidrBlock = "0.0.0.0/0";
      gatewayId = resources.vpcInternetGateways.adapayIGW;
      tags = { inherit environment; };
    };
  };
}
