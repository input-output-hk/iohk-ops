{ config, ... }: {
  imports = [
    ./common.nix
    ./hydra-master-common.nix
  ];

  services.dd-agent.tags = [ "role:hydra" "depl:${config.deployment.name}" ];
}
