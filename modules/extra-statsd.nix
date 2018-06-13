{ pkgs, lib, config, ... }:

with lib;
let
  extra-statsd = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "cleverca22";
    repo = "extra-statsd";
    rev = "930d10e9698e772eaa4d97e00789bef3cf105591";
    sha256 = "0nn5vy4j95bcfsa9xf6mfaqxbybnpb3z0pd3ispdxzcby79f197f";
  }) {};
in {
  options = {
    services.extra-statsd = mkEnableOption "extra-statsd";
  };
  config = mkIf config.services.extra-statsd {
    systemd.services.extra-statsd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "dd-agent.service" ];
      script = ''
        sleep 30
        exec ${extra-statsd.static}/bin/extra-statsd
      '';
    };
  };
}
