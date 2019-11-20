{ name, pkgs, ... }:

{
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
    autoPrune.dates = "daily";
    autoPrune.flags = [ "--all" "--force" ];
  };

  # Provide dockerhub credentials to buildkite
  systemd.services.buildkite-agent-setup-docker = {
    wantedBy = [ "buildkite-agent.service" ];
    script = ''
      mkdir -p ~buildkite-agent/.docker
      ln -sf /run/keys/dockerhub-auth ~buildkite-agent/.docker/config.json
      chown -R buildkite-agent:nogroup ~buildkite-agent/.docker
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };
}
