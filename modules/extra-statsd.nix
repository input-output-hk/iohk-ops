{ pkgs, lib, config, ... }:

with lib;
let
  extra-statsd = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "cleverca22";
    repo = "extra-statsd";
    rev = "bb4d394b224184f59369d44abecd455c4ca8195b";
    sha256 = "048r26p853scf5ms71lqa5h97jg1fqbq1dphdx7ink0mdw6hqjpa";
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
