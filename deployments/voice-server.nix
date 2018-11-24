let
  accessKeyId = "cardano-deployer";
  region = "eu-central-1";
in {
  voice-server = { config, resources, ... }: {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        keyPair = resources.ec2KeyPairs.voice-server-key;
        instanceType = "t3.micro";
        securityGroups = [ resources.ec2SecurityGroups.voice-server-firewall ];
      };
      route53.hostName = "voice-server.aws.iohkdev.io";
    };
    services = {
      teamspeak3 = {
        enable = true;
      };
    };
    nixpkgs.config.allowUnfree = true;
    networking = {
      firewall = {
        allowedTCPPorts = with config.services.teamspeak3; [ fileTransferPort queryPort ];
        allowedUDPPorts = with config.services.teamspeak3; [ defaultVoicePort ];
      };
    };
  };
  resources.ec2KeyPairs.voice-server-key = {
    inherit region accessKeyId;
  };
  resources.ec2SecurityGroups.voice-server-firewall = {
    inherit region accessKeyId;
    rules = [
      { protocol = "tcp"; fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
      { protocol = "tcp"; fromPort = 30033; toPort = 30033; sourceIp = "0.0.0.0/0"; }
      { protocol = "tcp"; fromPort = 10011; toPort = 10011; sourceIp = "0.0.0.0/0"; }
      { protocol = "udp"; fromPort = 9987; toPort = 9987; sourceIp = "0.0.0.0/0"; }
    ];
  };
}
