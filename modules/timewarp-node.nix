{ sender ? false }:
{ resources, pkgs, ...}: 

let
  time-warp = (import ./../srk-nixpkgs/default.nix { inherit pkgs; }).time-warp;
  generatingAMI = builtins.getEnv "GENERATING_AMI";
in {
  imports = [ (import ./common.nix) ];

  users = {
    users.timewarp = {
      description     = "";
      group           = "timewarp";
      createHome      = true;
      isNormalUser = true;
    };
    groups.timewarp = { };
  };

  networking.firewall.allowedTCPPorts = [ 3055 ];

  systemd.services.timewarp = {
    description   = "";
    wantedBy      = [ "multi-user.target" ];
    after         = [ "network.target" ];
    serviceConfig = {
      User = "timewarp";
      Group = "timewarp";
      Restart = "always";
      KillSignal = "SIGINT";
      PrivateTmp = true;
      ExecStart =
       if sender
       then toString [
              "/bin/sh -c \"${time-warp}/bin/bench-sender "
              "+RTS -N -RTS "
              "--log-config /home/timewarp/sender-logging.yaml "
              "--logs-prefix /home/timewarp "
              "`cat /home/timewarp/peers.txt` "
              "-r 90 -m 10800 -d 130 \""
            ]
       else toString [
              "${time-warp}/bin/bench-receiver "
              "+RTS -N -RTS "
              "--log-config /home/timewarp/receiver-logging.yaml "
              "--logs-prefix /home/timewarp "
              "-p 3055 -d 150 "
            ];
    };
  };
} // lib.optionalAttrs (generatingAMI != "1") {
  deployment.ec2.region = "eu-central-1";
  deployment.ec2.keyPair = resources.ec2KeyPairs.cardano-test-eu;
};
