let
  accessKeyId = "cardano-deployer";
  region = "eu-central-1";
in {
  voice-server = { config, resources, ... }: {
    deployment = {
      ec2 = {
        inherit region accessKeyId;
        keyPair = resources.ec2KeyPairs.voice-server;
        instanceType = "t3.nano";
      };
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
  resources.ec2KeyPairs.voice-server = {
    inherit region accessKeyId;
  };
}
